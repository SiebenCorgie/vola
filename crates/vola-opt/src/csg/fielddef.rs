use rvsdg::{edge::InputType, region::RegionLocation, smallvec::SmallVec, NodeRef};
use vola_ast::{
    alge::LetStmt,
    common::{Ident, TypedIdent},
    csg::{CSGBinding, CSGStmt},
};
use vola_common::{report, Span};

use crate::{
    ast::{AstLambdaBuilder, LambdaBuilderCtx},
    common::{LmdContext, Ty, VarDef},
    error::OptError,
    Optimizer,
};

pub struct FieldDef {
    pub span: Span,
    pub name: Ident,
    pub input_signature: SmallVec<[(Ident, Ty); 3]>,

    ///The λ-Node of this fielddef
    pub lambda: NodeRef,
    ///Shortcut to the λ-Node's inner region.
    pub lambda_region: RegionLocation,
}

impl LambdaBuilderCtx for FieldDef {}

impl FieldDef {
    fn build_block(
        mut self,
        mut lmd_builder: AstLambdaBuilder,
        fielddef: vola_ast::csg::FieldDef,
    ) -> Result<Self, OptError> {
        //First process all stmts in order.
        // then hook them up to the access expressions at the end

        for stmt in fielddef.stmts {
            match stmt {
                CSGStmt::CSGBinding(csgbinding) => {
                    self.setup_csg_binding(&mut lmd_builder, csgbinding)?
                }
                CSGStmt::LetStmt(letstmt) => self.setup_csg_let(&mut lmd_builder, letstmt)?,
            }
        }

        //We end by binding the _final_ tree to the output
        let last_output = lmd_builder.setup_csg_tree(fielddef.ret, &mut self)?;
        let result_index = lmd_builder
            .opt
            .graph
            .node_mut(self.lambda)
            .node_type
            .unwrap_lambda_mut()
            .add_result();
        //and wire it to the output
        lmd_builder.opt.graph.on_region(&self.lambda_region, |reg| {
            let _ = reg
                .connect_to_result(last_output, InputType::Result(result_index))
                .unwrap();
        });

        Ok(self)
    }

    //TODO Unify this and the one of export
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
            let err = OptError::AnySpannedWithSource {
                source_span: existing.span.clone().into(),
                source_text: "first defined here".to_owned(),
                text: format!("
cannot redefine variable with name \"{}\".
Note that vola does not support shadowing. If you just want to change the value of that variable, consider doing it like this:
`{} = ...;`",
                              decl_name.0, decl_name.0),
                span: span.clone().into(),
                span_text: "tried to redefine here".to_owned() };
            report(err.clone(), span.get_file());
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

        Ok(())
    }

    //TODO Unify this and the one of export
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
            let err = OptError::AnySpannedWithSource {
                source_span: existing.span.clone().into(),
                source_text: "first defined here".to_owned(),
                text: format!("
cannot redefine variable with name \"{}\".
Note that vola does not support shadowing. If you just want to change the value of that variable, consider doing it like this:
`{} = ...;`",
                              decl_name.0, decl_name.0),
                span: span.clone().into(),
                span_text: "tried to redefine here".to_owned() };
            report(err.clone(), span.get_file());
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

        Ok(())
    }
}

impl Optimizer {
    pub fn add_field_def(
        &mut self,
        fielddef: vola_ast::csg::FieldDef,
    ) -> Result<NodeRef, OptError> {
        let mut input_signature = SmallVec::new();
        for typed_ident in fielddef.inputs.iter() {
            let ty: Ty = match typed_ident.ty.clone().try_into() {
                Ok(ty) => ty,
                Err(e) => {
                    report(e.clone(), typed_ident.span.get_file());
                    return Err(e);
                }
            };

            input_signature.push((typed_ident.ident.clone(), ty));
        }

        //Setup the λ-Region. Always export
        let (lambda, lambda_region) = self.graph.on_omega_node(|omg| {
            omg.new_function(true, |lmd_builder| {
                lmd_builder.on_region(|reg| reg.parent_location())
            })
        });

        let lmd_context =
            LmdContext::new_for_fielddef(&mut self.graph, &mut self.typemap, lambda, &fielddef);

        //Temporary builder that tracks things like the defined variables etc.
        // Is dropped within the concept_impl.build_block()
        let lmd_builder = AstLambdaBuilder {
            opt: self,
            lmd_context,
            lambda,
            lambda_region,
        };

        let new_def = FieldDef {
            span: fielddef.span.clone(),
            name: fielddef.name.clone(),
            input_signature,
            lambda,
            lambda_region,
        };

        let interned_fielddef = new_def.build_block(lmd_builder, fielddef)?;

        let fieldef_name = interned_fielddef.name.0.clone();
        self.field_def.insert(fieldef_name, interned_fielddef);

        Ok(lambda)
    }
}
