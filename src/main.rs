use std::{collections::HashMap, error, fs::read_to_string};

use ariadne::{Label, Report, Source};
use cfg::CfgStatics;
use la_arena::Arena;
use lang_c::{
    ast::{DeclaratorKind, ExternalDeclaration, Identifier},
    driver::{parse, Config},
};
use lower::{lower_body, lower_statics};

use crate::{
    check::{check_cfg, CheckError, LeakByAssign, LeakByReturn, UseAfterMove},
    utils::SpanUtils,
};

mod cfg;
mod check;
mod lower;
mod utils;

// const TEST_FILE: &str = "malloc_leak1.c";
const TEST_FILE: &str = "use_after_move3.c";

fn main() {
    let config = Config::default();
    let ast = parse(&config, format!("examples/{}", TEST_FILE)).unwrap();
    let mut statics = CfgStatics {
        name_to_static: HashMap::new(),
        statics: Arena::new(),
    };
    for node in &ast.unit.0 {
        match &node.node {
            ExternalDeclaration::Declaration(decl) => {
                lower_statics(&decl.node, &mut statics);
                for decls in &decl.node.declarators {
                    if let DeclaratorKind::Identifier(id) = &decls.node.declarator.node.kind.node {
                        Report::build(ariadne::ReportKind::Advice, TEST_FILE, 0)
                            .with_message(format!("there is a decl here named {}", id.node.name))
                            .with_label(
                                Label::new((TEST_FILE, id.span.start..id.span.end))
                                    .with_message("it is here")
                                    .with_color(ariadne::Color::Cyan),
                            )
                            .finish()
                            .eprint((TEST_FILE, Source::from(&ast.source)));
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
                        println!("{:#?}", cfg);
                        let errors = check_cfg(&cfg);
                        for error in errors {
                            match error {
                                CheckError::LeakByAssign(LeakByAssign(
                                    own_intro_span,
                                    own_moved_span,
                                    lost_span,
                                )) => {
                                    let mut builder = Report::build(ariadne::ReportKind::Error, TEST_FILE, 0)
                                        .with_message("Potential leak detected")
                                        .with_label(
                                            Label::new((
                                                TEST_FILE,
                                                own_intro_span.start..own_intro_span.end,
                                            ))
                                            .with_message("Ownership of the value transferred to this function here")
                                            .with_color(ariadne::Color::Cyan),
                                        );

                                    if let Some(own_moved_span) = own_moved_span {
                                        if !own_moved_span.contains(&own_intro_span) {
                                            builder.add_label(Label::new((
                                                TEST_FILE,
                                                    own_moved_span.start..own_moved_span.end,
                                                ))
                                                .with_message("Ownership of the value transferred to this place here")
                                                .with_color(ariadne::Color::Cyan)
                                            )
                                        }
                                    }

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
                                        .eprint((TEST_FILE, Source::from(&ast.source)));
                                }
                                CheckError::LeakByReturn(LeakByReturn(
                                    own_span,
                                    _,
                                    return_span,
                                )) => {
                                    Report::build(ariadne::ReportKind::Error, TEST_FILE, 0)
                                        .with_message("Potential leak detected")
                                        .with_label(
                                            Label::new((
                                                TEST_FILE,
                                                own_span.start..own_span.end,
                                            ))
                                            .with_message("Ownership of the value transferred to this function here")
                                            .with_color(ariadne::Color::Cyan),
                                        )
                                        .with_label(
                                            Label::new((
                                                TEST_FILE,
                                                return_span.start..return_span.end,
                                            ))
                                            .with_message("Function exited without proper destruction of that value")
                                            .with_color(ariadne::Color::Red),
                                        )
                                        .finish()
                                        .eprint((TEST_FILE, Source::from(&ast.source)));
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
                                        .eprint((TEST_FILE, Source::from(&ast.source)));
                                }
                            }
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
