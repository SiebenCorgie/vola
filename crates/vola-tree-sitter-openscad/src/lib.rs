//! TreeSitter based OpenScad->vola parser. Transforms the SCAD input into a Vola-Ast, if possible.

mod error;
use std::path::{Path, PathBuf};

use error::ParserError;
use scad_ast::{ScadBlock, ScadStmt, ScadTopLevel};
use tree_sitter::{Node, Parser};
use vola_ast::VolaAst;
use vola_common::{
    FileString, Span,
    ariadne::Label,
    error::{error_reporter, warning_reporter},
    report,
};

mod assignment;
mod comment;
mod convert;
mod expr;
mod normalize;
mod scad_ast;
mod stmt;
mod util;

pub fn report_here(error: impl ToString, span: Span) {
    report(
        error_reporter(error, span.clone())
            .with_label(Label::new(span).with_message("here"))
            .finish(),
    );
}

pub fn warn_here(warn: impl ToString, span: Span) {
    report(
        warning_reporter(warn, span.clone())
            .with_label(Label::new(span).with_message("here"))
            .finish(),
    );
}

///Context on the parser, like the current src file, and errors that occured, but are ignored.
pub struct ParserCtx {
    deep_errors: Vec<ParserError>,
    pub deep_warnings: Vec<(Span, String)>,
    src_file: FileString,

    ///See the SCAD documentation on both resolve behaviors: [here](https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/Include_Statement)
    ///Collects all `use` paths that where encountered.
    resolve_uses: Vec<PathBuf>,
    ///Collects all `include` paths that where encountered.
    resolve_includes: Vec<PathBuf>,
}

impl ParserCtx {
    pub fn new(file: FileString) -> ParserCtx {
        ParserCtx {
            deep_errors: Vec::new(),
            deep_warnings: Vec::new(),
            src_file: file,
            resolve_uses: Vec::with_capacity(0),
            resolve_includes: Vec::with_capacity(0),
        }
    }
    pub fn new_fileless() -> ParserCtx {
        ParserCtx {
            deep_errors: Vec::new(),
            deep_warnings: Vec::new(),
            src_file: FileString::default(),
            resolve_uses: Vec::with_capacity(0),
            resolve_includes: Vec::with_capacity(0),
        }
    }
    ///Creates a new span for `node` on this context.
    pub fn span(&self, node: &Node) -> Span {
        Span::from(node).with_file(&self.src_file)
    }

    pub fn get_file(&self) -> Option<&str> {
        if self.src_file.is_empty() {
            None
        } else {
            Some(self.src_file.as_str())
        }
    }

    pub fn try_resolve_local_path(&self, path: impl AsRef<Path>) -> Result<PathBuf, ParserError> {
        let path = path.as_ref();
        //first try local to _our_ path, then try to read the OPENSCADPATH EnvVar

        //If the path is already absolute, just return it
        if path.is_absolute() {
            return Ok(path.to_owned());
        }

        if let Some(base_file) = self.get_file() {
            let base_path = Path::new(base_file).to_path_buf();
            let full_path = base_path.join(path);
            if full_path.exists() {
                return Ok(full_path);
            }
        }

        //was not relative to the file we opened, try to read the env-var and build such a path
        if let Ok(ev_path) = std::env::var("OPENSCADPATH") {
            let evpath = PathBuf::from(ev_path);
            if !evpath.exists() {
                return Err(ParserError::FSError(format!(
                    "OPENSCADPATH=\"{:?}\" does not exist",
                    evpath
                )));
            }

            let full_path = evpath.join(path);
            if full_path.exists() {
                return Ok(full_path);
            }
        }

        Err(ParserError::FSError(format!(
            "Could not find {:?}, neither relative to {:?} nor relative to $OPENSCADPATH",
            path,
            self.get_file().unwrap_or("NO-SOURCE-FILE-LOCATION")
        )))
    }
}

///Parses `file`. Returns the [VolaAst] on success, or a partially parsed AST, and the reported errors, if any
/// parsing errors happened.
pub fn parse_file(file: impl AsRef<Path>) -> Result<VolaAst, (VolaAst, Vec<ParserError>)> {
    let dta = match std::fs::read(file.as_ref()) {
        Ok(dta) => dta,
        Err(e) => {
            let err = ParserError::FSError(e.to_string());
            vola_common::report(error_reporter(err.clone(), Span::empty()).finish());

            return Err((
                VolaAst {
                    entries: Vec::with_capacity(0),
                },
                vec![err],
            ));
        }
    };
    let file_src_str = file.as_ref().to_str().unwrap_or("NonUnicodeFilename");
    parse_data(&dta, Some(file_src_str.into()))
}

///Parses `string`. Returns the [VolaAst] on success, or a partially parsed AST, and the reported errors, if any
/// parsing errors happened.
pub fn parse_string(string: String) -> Result<VolaAst, (VolaAst, Vec<ParserError>)> {
    let as_bytes = string.as_bytes();
    parse_data(as_bytes, None)
}

pub fn parse_from_bytes(bytes: &[u8]) -> Result<VolaAst, (VolaAst, Vec<ParserError>)> {
    parse_data(bytes, None)
}

//load the vola-lang tree-sitter grammar / parser
fn parser() -> Parser {
    let mut parser = Parser::new();
    parser
        .set_language(tree_sitter_openscad::language())
        .expect("Failed to load tree-sitter-openscad");
    parser
}

pub struct OpenScadTreeSitterParser;

impl vola_ast::VolaParser for OpenScadTreeSitterParser {
    fn parse_from_byte(
        &self,
        src_file: Option<FileString>,
        byte: &[u8],
    ) -> Result<VolaAst, vola_ast::AstError> {
        #[allow(unused_variables)]
        parse_data(byte, src_file.clone()).map_err(|(partial_ast, e)| {
            vola_ast::AstError::ParsingError {
                path: src_file
                    .map(|f| f.to_string())
                    .unwrap_or("NoFile".to_owned()),
                err: format!("Parser had {} errors", e.len()),
            }
        })
    }
}

fn parse_data(
    data: &[u8],
    src_file: Option<FileString>,
) -> Result<VolaAst, (VolaAst, Vec<ParserError>)> {
    let mut empty_ast = VolaAst {
        entries: Vec::with_capacity(0),
    };

    //parse the ScadAst _fully_. This will also take care of recursively resolving any used modules
    let mut scad_ast =
        parse_data_scad(data, src_file).map_err(|(_scad_ast, e)| (empty_ast.clone(), e))?;

    //At this point we have a fully parsed, (and valid) ScadAST. Now is the time to _somehow_ transform that into volacode.
    //This includes several ScadAST transformations, in order to _cleanup_ semantics for us.
    //
    //The main pain-point is [scope-of-variables](https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/General#Scope_of_variables)
    //namely that they seem to be _set-on-last-write-in-block_
    //
    //Finally we transform the _normalized_ ScadAst into the Vola Ast. This is also where we sort out any undefined function calls.
    //There are two options for each:
    // 1. Its calling something unsupported like `dxf_dim(file="..", ..)`, in that case we bail,
    // 2. Its calling some kind of supported domain specific _thing_, i.e. `color(some_vec_3)`. Is that case we insert the appropriate
    //    CSGOperation. We maintain a Scad compatible standard library, that (tries to) mirrors oScad's functionality like union, intersect, color, translate etc.
    scad_ast.normalize().map_err(|e| (empty_ast.clone(), e))?;
    scad_ast.into_vola_ast().map_err(|e| (empty_ast, e))
}

///Internal parser implementation
fn parse_data_scad(
    data: &[u8],
    src_file: Option<FileString>,
) -> Result<ScadTopLevel, (ScadTopLevel, Vec<ParserError>)> {
    let mut ctx = if let Some(src) = src_file {
        ParserCtx::new(src)
    } else {
        ParserCtx::new_fileless()
    };

    //recursively parse all nodes
    let mut parser = parser();
    let syn_tree = match parser.parse(&data, None) {
        None => {
            let err = ParserError::TreeSitterFailed;
            report(error_reporter(err.clone(), Span::empty()).finish());
            return Err((
                scad_ast::ScadTopLevel {
                    main: ScadBlock {
                        stmts: Vec::with_capacity(0),
                        span: Span::empty(),
                    },
                    modules: Vec::with_capacity(0),
                },
                vec![err],
            ));
        }
        Some(syntree) => syntree,
    };

    //NOTE: SCAD Has _out-of-module_ statements. This can be seen as something like a _main_
    //      function that catches all statements that are not part of any module.
    //
    //      To make this work with vola we _just_ yeet any non-module statement into the _main-block_
    //      once the module has finished parsing (and all sub-modules), we merge all _main-functions_, and
    //      serialize it as the _main-λ-with-csg-return-arg_. That way the _user_ of the scad module can _use_
    //      the generated CSG by _using_ the main function.
    //
    //      The behavior might change later on, since we could also directly build a export function that returns the SDF
    //      value of the _main_ function.
    let mut tl = scad_ast::ScadTopLevel::empty(&ctx, &syn_tree.root_node());

    let mut cursor = syn_tree.root_node().walk();
    //Collects args until a non-comment, non-ct-arg node is returned. Attaches all preceeding ctargs
    // to that toplevel node.
    for node in syn_tree.root_node().children(&mut cursor) {
        match node.kind() {
            "comment" => {
                let stmt = ScadStmt::Comment(comment::comment(&mut ctx, data, &node));
                tl.main.stmts.push(stmt);
            }
            "use_statement" => {
                if let Err(e) = stmt::file_use_stmt(&mut ctx, true, data, &node) {
                    report_here(e, ctx.span(&node));
                }
            }
            "include_statement" => {
                if let Err(e) = stmt::file_use_stmt(&mut ctx, false, data, &node) {
                    report_here(e, ctx.span(&node));
                }
            }
            "module_deceleration" => {
                todo!()
            }
            "function_declecration" => {
                todo!()
            }
            "assignment" => {
                match assignment::assignment(&mut ctx, data, &node) {
                    Ok(assignment) => {
                        //push into main, and return _None_
                        tl.main.stmts.push(ScadStmt::Assign(assignment));
                    }
                    Err(ParserError::Ignored) => {}
                    Err(e) => {
                        report_here(e.clone(), ctx.span(&node));
                        ctx.deep_errors.push(e);
                    }
                }
            }
            //Happens on the top-level after working on a _to-be-added-to-main_ stmt.
            ";" => {}
            other => {
                //if it is not your standard _include / use / module / function_, it is probably a _out-of-module_
                //statement. Try to parse that. If that works, append it to the _main_ block. Otherwise
                // emit an error
                match stmt::stmt(&mut ctx, data, &node) {
                    Ok(stmt) => tl.main.stmts.push(stmt),
                    //Was not a _valid_ statement, print the error, then also emit the _invalid_ error
                    Err(e) => {
                        if e != ParserError::Ignored {
                            report_here(e.clone(), ctx.span(&node));
                            ctx.deep_errors
                                .push(ParserError::Unexpected(other.to_owned()))
                        }
                    }
                }
            }
        }
    }

    //resolve uses by parsing AST recursively, but only copying over modules and functions
    //resolve includes by parsing AST recursively and merging main function.
    for (path, is_include) in ctx
        .resolve_uses
        .into_iter()
        .map(|use_path| (use_path, false))
        .chain(ctx.resolve_includes.into_iter().map(|incl| (incl, true)))
    {
        match std::fs::read(&path).map_err(|e| ParserError::FSError(e.to_string())) {
            Err(e) => ctx.deep_errors.push(e),
            Ok(content) => {
                let mut sub_ast =
                    parse_data_scad(&content, Some(FileString::from_str(path.to_str().unwrap())))?;

                if is_include {
                    tl.main.stmts.append(&mut sub_ast.main.stmts);
                }

                tl.modules.append(&mut sub_ast.modules);
                println!("Merge function!");
            }
        }
    }

    for (span, warning) in ctx.deep_warnings {
        report(
            warning_reporter(warning, span.clone())
                .with_label(Label::new(span).with_message("here"))
                .finish(),
        );
    }

    if ctx.deep_errors.len() > 0 {
        Err((tl, ctx.deep_errors))
    } else {
        Ok(tl)
    }
}
