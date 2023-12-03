use std::{eprintln, process::exit};

use ariadne::{Label, Report, Source};
use cfg::CfgStatics;
use clap::Parser;
use lang_c::{
    ast::{DeclaratorKind, ExternalDeclaration},
    driver::{parse, Config},
};
use lower::lower_body;

use crate::check::{check_cfg, CheckError, LeakByAssign, LeakByReturn, UseAfterMove};

mod cfg;
mod check;
mod lower;
mod report;
mod utils;

const TEST_FILE: &str = "preproccessed_file.c";

#[derive(Parser)]
struct CliOptions {
    #[clap(
        value_name = "INPUT",
        required = true,
        help = "Sets the input file to compile"
    )]
    input: String,

    #[clap(long)]
    debug_mode: bool,

    #[clap(short = 'I')]
    include_directories: Vec<String>,
}

fn main() {
    let cli_options = CliOptions::parse();
    let mut config = Config::default();
    config.cpp_options.extend(
        cli_options
            .include_directories
            .iter()
            .map(|x| format!("-I{x}")),
    );
    let ast = match parse(&config, cli_options.input) {
        Ok(x) => x,
        Err(e) => {
            eprintln!("Error in running the preprocessor:\n{e}");
            exit(1);
        }
    };
    let mut statics = CfgStatics::new();
    let debug_mode = cli_options.debug_mode;
    if debug_mode {
        std::fs::write("/tmp/corrow-checker-preprocessed-file.c", &ast.source).unwrap();
    }
    for node in &ast.unit.0 {
        match &node.node {
            ExternalDeclaration::Declaration(decl) => {
                statics.lower_statics(&decl.node).unwrap();
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

                if !statics.name_to_static.contains_key(&name) {
                    statics.lower_declarator(&fd.node.declarator).unwrap();
                }

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
                                CheckError::LeakByReturn(LeakByReturn(
                                    own_reason,
                                    return_span,
                                    return_implicit,
                                )) => {
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
                                            .with_message(if return_implicit {
                                                "Function implicitly exited without proper destruction of that value"
                                            } else {
                                                "Function exited without proper destruction of that value"    
                                            })
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
                        let fn_span = fd.node.declarator.node.kind.span;
                        let mut builder =
                            Report::build(ariadne::ReportKind::Error, TEST_FILE, fn_span.start)
                                .with_message("Cfg lowering failed");
                        error.add_to_report(&mut builder);
                        builder
                            .with_label(
                                Label::new((TEST_FILE, fn_span.start..fn_span.end))
                                    .with_message("In this function")
                                    .with_color(ariadne::Color::Red),
                            )
                            .finish()
                            .eprint((TEST_FILE, Source::from(&ast.source)))
                            .unwrap();
                    }
                }
            }
        }
    }
}
