use lang_c::{driver::{Config, parse}, ast::{ExternalDeclaration, Identifier, DeclaratorKind}};
use lower::lower_body;

mod cfg;
mod lower;

fn main() {
    let config = Config::default();
    let ast = parse(&config, "examples/cf_4c.c").unwrap();
    for node in &ast.unit.0 {
        match &node.node {
            ExternalDeclaration::Declaration(decl) => {
                for decls in &decl.node.declarators {
                    if let DeclaratorKind::Identifier(id) = &decls.node.declarator.node.kind.node {
                        println!("decl {}", id.node.name);
                    }
                }
            },
            ExternalDeclaration::StaticAssert(_) => (),
            ExternalDeclaration::FunctionDefinition(fd) => {
                if let DeclaratorKind::Identifier(id) = &fd.node.declarator.node.kind.node {
                    println!("fn {}", id.node.name);
                } else {
                    unreachable!();
                }
                match lower_body(&fd.node) {
                    Ok(cfg) => {
                        println!("{:#?}", cfg);
                    },
                    Err(error) => {
                        println!("mir lowering failed: {}", error);
                    },
                }
            },
        }
    }
}
