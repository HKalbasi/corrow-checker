use std::error;

use lang_c::{
    ast::{DeclaratorKind, ExternalDeclaration, Identifier},
    driver::{parse, Config},
};
use lower::lower_body;

use crate::check::check_cfg;

mod cfg;
mod lower;
mod check;

fn main() {
    let config = Config::default();
    let ast = parse(&config, "examples/malloc_leak1.c").unwrap();
    for node in &ast.unit.0 {
        match &node.node {
            ExternalDeclaration::Declaration(decl) => {
                for decls in &decl.node.declarators {
                    if let DeclaratorKind::Identifier(id) = &decls.node.declarator.node.kind.node {
                        println!("decl {}", id.node.name);
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
