use ahash::AHashMap;
/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use rvsdg::{
    edge::{InportLocation, InputType, OutportLocation, OutputType},
    region::RegionLocation,
    smallvec::SmallVec,
    NodeRef, SmallColl,
};
use vola_ast::{
    alge::LetStmt,
    common::Ident,
    csg::{CSGBinding, CSGStmt},
};

use crate::{
    ast::{
        block_builder::{BlockBuilder, FetchStmt, ReturnExpr},
        AstLambdaBuilder, LambdaBuilderCtx,
    },
    common::{LmdContext, Ty, VarDef},
    error::OptError,
    OptNode, Optimizer,
};
use vola_common::{ariadne::Label, error::error_reporter, report, Span};

use super::TreeAccess;

///All meta information to an export_fn.
pub struct ExportFn {
    pub span: Span,
    ///The input signature that is exported to the linkage-attributes
    #[allow(unused)]
    pub input_signature: SmallVec<[(Ident, Ty); 3]>,
    ///The outputs that are exported from the module.
    pub output_signature: SmallVec<[Ty; 3]>,

    ///Name of the function, also the one we use to export it.
    pub ident: Ident,

    ///The λ-Node of this concept
    pub lambda: NodeRef,
    ///Shortcut to the λ-Node's inner region.
    pub lambda_region: RegionLocation,
}

impl LambdaBuilderCtx for ExportFn {}

impl ExportFn {
    fn build_block(
        mut self,
        mut lmd_builder: AstLambdaBuilder,
        exportfn: vola_ast::csg::ExportFn,
    ) -> Result<Self, OptError> {
        //First process all stmts in order.
        // then hook them up to the access expressions at the end

        for stmt in exportfn.stmts {
            match stmt {
                CSGStmt::CSGBinding(csgbinding) => {
                    self.setup_csg_binding(&mut lmd_builder, csgbinding)?
                }
                CSGStmt::LetStmt(letstmt) => self.setup_csg_let(&mut lmd_builder, letstmt)?,
            }
        }

        //Now wire up all access descriptors
        for access in exportfn.access_descriptors {
            self.wire_access(&mut lmd_builder, access)?;
        }

        Ok(self)
    }

    fn wire_access(
        &mut self,
        builder: &mut AstLambdaBuilder,
        access: vola_ast::csg::AccessDesc,
    ) -> Result<(), OptError> {
        //check if we can find the tree.
        let field_src = match builder.lmd_context.defined_vars.get(&access.tree_ref.0) {
            Some(field_src) => field_src,
            None => {
                return Err(OptError::report_variable_not_found(
                    &access.span,
                    &access.tree_ref.0,
                ));
            }
        };

        let concept = match builder.opt.concepts.get(&access.call.ident.0) {
            None => {
                return Err(OptError::report_no_concept(
                    &access.span,
                    &access.call.ident.0,
                ));
            }
            Some(con) => con,
        };

        //While at it, make sure the call-parameter-count matches
        if concept.src_ty.len() != access.call.args.len() {
            return Err(OptError::report_argument_missmatch(
                &concept.span,
                concept.src_ty.len(),
                &access.span,
                access.call.args.len(),
            ));
        }

        let concept_name = concept.name.clone();

        //We use the concept's return type to infer the output signature.
        let return_type: Ty = concept.dst_ty.clone().into();

        //register in output signature
        self.output_signature.push(return_type.clone());

        //At this point we can be sure that the concept exists, and is at least called with the right amount
        // of arguments.
        // We can also be sure that a variable with the given name exists. Though we are not sure its a field
        // (yet?).
        // TODO: We could already tag the csg output with a CSG type, so we can be sure already.
        //      Or we just do that at the type-derive / type_check stage.

        let mut wires: SmallVec<[OutportLocation; 3]> = SmallVec::new();
        wires.push(field_src.port.clone());

        let mut signature = SmallVec::new();
        //First argument must alwoas be the tree thats called.
        signature.push(Ty::CSGTree);
        //all following args are the callargs
        for arg in concept.src_ty.iter() {
            signature.push(
                arg.clone()
                    .try_into()
                    .expect("Could not convert tree call arg to opttype"),
            );
        }
        for arg in access.call.args {
            let arg_port = builder.setup_alge_expr(arg, self)?;
            wires.push(arg_port);
        }

        //add an result port to the lambda node
        let resultidx = builder
            .opt
            .graph
            .node_mut(self.lambda)
            .node_type
            .unwrap_lambda_mut()
            .add_result();

        let _access_output = builder
            .opt
            .graph
            .on_region(&self.lambda_region, |reg| {
                let (node, _) = reg
                    .connect_node(
                        OptNode::new(
                            TreeAccess::new(concept_name, signature, return_type),
                            access.span.clone(),
                        ),
                        &wires,
                    )
                    .unwrap();

                let _ = reg
                    .connect_to_result(node.output(0), InputType::Result(resultidx))
                    .unwrap();
                node.output(0)
            })
            .unwrap();

        Ok(())
    }

    fn setup_csg_binding(
        &mut self,
        builder: &mut AstLambdaBuilder,
        binding: CSGBinding,
    ) -> Result<(), OptError> {
        let CSGBinding {
            span,
            decl_name,
            tree,
        } = binding;

        //Similar to let statements, make sure that no variable exists with the given name.
        // If so, build the csg tree

        if builder.lmd_context.var_exists(&decl_name.0) {
            let existing = builder.lmd_context.defined_vars.get(&decl_name.0).unwrap();
            let err = OptError::Any {
                text: format!("
cannot redefine variable with name \"{}\".
Note that vola does not support shadowing. If you just want to change the value of that variable, consider doing it like this:
`{} = ...;`",
                              decl_name.0, decl_name.0),
                      };
            report(
                error_reporter(err.clone(), span.clone())
                    .with_label(
                        Label::new(existing.span.clone()).with_message("first defined here"),
                    )
                    .with_label(Label::new(span.clone()).with_message("tried to redefined here"))
                    .finish(),
            );
            return Err(err);
        }

        let def_port = builder.setup_csg_tree(tree, self)?;

        //register in the lmd context
        builder.lmd_context.add_define(
            decl_name.0,
            VarDef {
                port: def_port,
                span,
            },
        );

        //register type for port
        builder.opt.typemap.set(def_port.into(), Ty::CSGTree);

        Ok(())
    }

    fn setup_csg_let(
        &mut self,
        builder: &mut AstLambdaBuilder,
        let_stmt: LetStmt,
    ) -> Result<(), OptError> {
        let LetStmt {
            span,
            decl_name,
            expr,
        } = let_stmt;

        if builder.lmd_context.var_exists(&decl_name.0) {
            let existing = builder.lmd_context.defined_vars.get(&decl_name.0).unwrap();
            let err = OptError::Any {
                text: format!("
cannot redefine variable with name \"{}\".
Note that vola does not support shadowing. If you just want to change the value of that variable, consider doing it like this:
`{} = ...;`",
                              decl_name.0, decl_name.0),
                      };
            report(
                error_reporter(err.clone(), span.clone())
                    .with_label(
                        Label::new(existing.span.clone()).with_message("first defined here"),
                    )
                    .with_label(Label::new(span.clone()).with_message("tried to redefined here"))
                    .finish(),
            );
            return Err(err);
        }

        let def_port = builder.setup_alge_expr(expr, self)?;

        //register in the lmd context
        builder.lmd_context.add_define(
            decl_name.0,
            VarDef {
                port: def_port,
                span,
            },
        );

        //NOTE: currently we don't know the type of the let's expression. This will be infered later on.

        Ok(())
    }
}

impl Optimizer {
    pub fn add_export_fn(
        &mut self,
        exportfn: vola_ast::csg::ExportFn,
    ) -> Result<NodeRef, OptError> {
        //TODO right now we can _kind_shadow_ concepts, operations, entities and filed
        // defs. So I guess it would be nice if we couldn't ?

        let mut input_signature = SmallVec::new();
        for typed_ident in exportfn.inputs.iter() {
            let ty: Ty = typed_ident.ty.clone().into();
            input_signature.push((typed_ident.ident.clone(), ty));
        }

        //Will be calculated later on
        let output_signature = SmallVec::new();

        //Setup the λ-Region. Always export
        let (lambda, lambda_region) = self.graph.on_omega_node(|omg| {
            omg.new_function(true, |lmd_builder| {
                lmd_builder.on_region(|reg| reg.parent_location())
            })
        });

        let lmd_ctx =
            LmdContext::new_for_exportfn(&mut self.graph, &mut self.typemap, lambda, &exportfn);

        let block_builder = BlockBuilder {
            span: exportfn.span.clone(),
            csg_operands: AHashMap::with_capacity(0),
            is_eval_allowed: false,
            return_type: SmallColl::new(),
            lmd_ctx,
            region: lambda_region,
            opt: self,
        };

        let stmts = exportfn
            .stmts
            .into_iter()
            .map(|stm| match stm {
                CSGStmt::LetStmt(l) => FetchStmt::Let(l),
                CSGStmt::CSGBinding(b) => FetchStmt::CSGBind(b),
            })
            .collect();
        let retexpr =
            ReturnExpr::AccessDescriptors(exportfn.access_descriptors.into_iter().collect());
        block_builder.build_block(stmts, retexpr)?;

        let exportfn = ExportFn {
            span: exportfn.span.clone(),
            input_signature,
            output_signature,
            ident: exportfn.name.clone(),

            lambda,
            lambda_region,
        };

        let export_name = exportfn.ident.0.clone();
        //Set the name of this function
        self.names.set(exportfn.lambda.into(), export_name.clone());
        self.span_tags
            .set(exportfn.lambda.into(), exportfn.span.clone());

        //add port type information
        for (inpidx, (_, inputty)) in exportfn.input_signature.iter().enumerate() {
            self.typemap.set(
                OutportLocation {
                    node: exportfn.lambda,
                    output: OutputType::Argument(inpidx),
                }
                .into(),
                inputty.clone(),
            );

            for edg in self
                .graph
                .node(exportfn.lambda)
                .node_type
                .unwrap_lambda_ref()
                .argument(inpidx)
                .unwrap()
                .edges
                .clone()
            {
                self.graph.edge_mut(edg).ty.set_type(inputty.clone());
            }
        }

        for (outpidx, ty) in exportfn.output_signature.iter().enumerate() {
            self.typemap.set(
                InportLocation {
                    node: exportfn.lambda,
                    input: InputType::Result(outpidx),
                }
                .into(),
                ty.clone(),
            );

            if let Some(edg) = self
                .graph
                .node(exportfn.lambda)
                .node_type
                .unwrap_lambda_ref()
                .result(outpidx)
                .unwrap()
                .edge
                .clone()
            {
                self.graph.edge_mut(edg).ty.set_type(ty.clone());
            }
        }

        self.export_fn.insert(export_name, exportfn);

        Ok(lambda)
    }
}
