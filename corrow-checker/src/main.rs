use std::collections::HashMap;

use ariadne::{Label, Report, Source};
use cfg::CfgStatics;
use la_arena::Arena;
use lang_c::{
    ast::{DeclaratorKind, ExternalDeclaration},
    driver::{parse, Config},
};
use lower::{lower_body, lower_statics};

use crate::check::{check_cfg, CheckError, LeakByAssign, LeakByReturn, UseAfterMove};

mod cfg;
mod check;
mod lower;
mod report;
mod utils;

// const TEST_FILE: &str = "malloc_leak1.c";
const TEST_FILE: &str = "malloc_leak1.c";

fn main() {
    let config = Config::default();
    let ast = parse(&config, std::env::args().nth(1).unwrap()).unwrap();
    let mut statics = CfgStatics {
        name_to_static: HashMap::new(),
        statics: Arena::new(),
    };
    let debug_mode = false; // FIXME: make this a flag
    for node in &ast.unit.0 {
        match &node.node {
            ExternalDeclaration::Declaration(decl) => {
                lower_statics(&decl.node, &mut statics).unwrap();
                for decls in &decl.node.declarators {
                    if let DeclaratorKind::Identifier(id) = &decls.node.declarator.node.kind.node {
                        if debug_mode {
                            Report::build(ariadne::ReportKind::Advice, TEST_FILE, 0)
                                .with_message(format!(
                                    "there is a decl here named {}",
                                    id.node.name
                                ))
                                .with_label(
                                    Label::new((TEST_FILE, id.span.start..id.span.end))
                                        .with_message("it is here")
                                        .with_color(ariadne::Color::Cyan),
                                )
                                .finish()
                                .eprint((TEST_FILE, Source::from(&ast.source)))
                                .unwrap();
                        }
                    }
                }
            }
            ExternalDeclaration::StaticAssert(_) => (),
            ExternalDeclaration::FunctionDefinition(fd) => {
                let name = match &fd.node.declarator.node.kind.node {
                    DeclaratorKind::Identifier(id) => id.node.name.clone(),
                    _ => unreachable!(),
                };

                match lower_body(name, &fd.node, &statics) {
                    Ok(cfg) => {
                        if debug_mode {
                            println!("{:#?}", cfg);
                        }
                        let mut errors = check_cfg(&cfg);
                        errors.sort_by_key(CheckError::compare_for_stability);
                        for error in errors {
                            match error {
                                CheckError::LeakByAssign(LeakByAssign(own_reason, lost_span)) => {
                                    let mut builder =
                                        Report::build(ariadne::ReportKind::Error, TEST_FILE, 0)
                                            .with_message("Potential leak detected");

                                    own_reason.add_to_report(&mut builder);

                                    builder
                                        .with_label(
                                            Label::new((
                                                TEST_FILE,
                                                lost_span.start..lost_span.end,
                                            ))
                                            .with_message(
                                                "Ownership handle lost without proper destruction",
                                            )
                                            .with_color(ariadne::Color::Red),
                                        )
                                        .finish()
                                        .eprint((TEST_FILE, Source::from(&ast.source)))
                                        .unwrap();
                                }
                                CheckError::LeakByReturn(LeakByReturn(own_reason, return_span)) => {
                                    let mut builder =
                                        Report::build(ariadne::ReportKind::Error, TEST_FILE, 0)
                                            .with_message("Potential leak detected");
                                    own_reason.add_to_report(&mut builder);
                                    builder
                                        .with_label(
                                            Label::new((
                                                TEST_FILE,
                                                return_span.start..return_span.end,
                                            ))
                                            .with_message("Function exited without proper destruction of that value")
                                            .with_color(ariadne::Color::Red),
                                        )
                                        .finish()
                                        .eprint((TEST_FILE, Source::from(&ast.source)))
                                        .unwrap();
                                }
                                CheckError::UseAfterMove(UseAfterMove(move_span, use_span)) => {
                                    Report::build(ariadne::ReportKind::Error, TEST_FILE, 0)
                                        .with_message("Potential use after move")
                                        .with_label(
                                            Label::new((TEST_FILE, move_span.start..move_span.end))
                                                .with_message("The value is moved here")
                                                .with_color(ariadne::Color::Cyan),
                                        )
                                        .with_label(
                                            Label::new((TEST_FILE, use_span.start..use_span.end))
                                                .with_message("The value is used here after move")
                                                .with_color(ariadne::Color::Red),
                                        )
                                        .finish()
                                        .eprint((TEST_FILE, Source::from(&ast.source)))
                                        .unwrap();
                                }
                            }
                        }
                    }
                    Err(error) => {
                        eprintln!("mir lowering failed: {}", error);
                    }
                }
            }
        }
    }
}
