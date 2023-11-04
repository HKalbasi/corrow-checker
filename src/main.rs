use std::{error, fs::read_to_string};

use ariadne::{Label, Report, Source};
use lang_c::{
    ast::{DeclaratorKind, ExternalDeclaration, Identifier},
    driver::{parse, Config},
};
use lower::lower_body;

use crate::check::check_cfg;

mod cfg;
mod check;
mod lower;

fn main() {
    let config = Config::default();
    let ast = parse(&config, "examples/malloc_leak1.c").unwrap();
    for node in &ast.unit.0 {
        match &node.node {
            ExternalDeclaration::Declaration(decl) => {
                for decls in &decl.node.declarators {
                    if let DeclaratorKind::Identifier(id) = &decls.node.declarator.node.kind.node {
                        Report::build(ariadne::ReportKind::Advice, "malloc_leak1.c", 0)
                            .with_message(format!("there is a decl here named {}", id.node.name))
                            .with_label(
                                Label::new(("malloc_leak1.c", id.span.start..id.span.end))
                                    .with_message("it is here")
                                    .with_color(ariadne::Color::Cyan),
                            )
                            .finish()
                            .eprint(("malloc_leak1.c", Source::from(&ast.source)));
                    }
                }
            }
            ExternalDeclaration::StaticAssert(_) => (),
            ExternalDeclaration::FunctionDefinition(fd) => {
                let name = match &fd.node.declarator.node.kind.node {
                    DeclaratorKind::Identifier(id) => id.node.name.clone(),
                    _ => unreachable!(),
                };

                match lower_body(name, &fd.node) {
                    Ok(cfg) => {
                        println!("{:#?}", cfg);
                        let errors = check_cfg(&cfg);
                        for error in errors {
                            println!("{:?}", error);
                        }
                    }
                    Err(error) => {
                        println!("mir lowering failed: {}", error);
                    }
                }
            }
        }
    }
}
