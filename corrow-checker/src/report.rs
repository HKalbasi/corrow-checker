use ariadne::Label;
use lang_c::span::Span;

use crate::{lower::CfgLowerError, TEST_FILE};

#[derive(Debug, Clone, Copy)]
pub struct OwnershipReason([Option<(Span, OwnershipReasonItemKind)>; 10]);

impl PartialEq for OwnershipReason {
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

impl Eq for OwnershipReason {}

impl std::hash::Hash for OwnershipReason {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        ().hash(state);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OwnershipReasonItemKind {
    MoveByAssign,
    MoveFromExternalFunction,
}

type ReportBuilder<'a> = ariadne::ReportBuilder<'a, (&'a str, std::ops::Range<usize>)>;

impl OwnershipReason {
    pub fn new(span: Span) -> Self {
        Self([None; 10]).push(span, OwnershipReasonItemKind::MoveFromExternalFunction)
    }

    pub fn with_move(self, move_span: Span) -> Self {
        self.push(move_span, OwnershipReasonItemKind::MoveByAssign)
    }

    fn push(mut self, span: Span, kind: OwnershipReasonItemKind) -> Self {
        if let Some(x) = self.0.iter_mut().find(|x| x.is_none()) {
            *x = Some((span, kind));
        }
        self
    }

    pub fn add_to_report(self, builder: &mut ReportBuilder<'_>) {
        for (span, kind) in self.0.iter().filter_map(|x| *x) {
            match kind {
                OwnershipReasonItemKind::MoveByAssign => builder.add_label(
                    Label::new((TEST_FILE, span.start..span.end))
                        .with_message("Ownership of the value transferred to this place here")
                        .with_color(ariadne::Color::Cyan),
                ),
                OwnershipReasonItemKind::MoveFromExternalFunction => builder.add_label(
                    Label::new((TEST_FILE, span.start..span.end))
                        .with_message("Ownership of the value transferred to this function here")
                        .with_color(ariadne::Color::Cyan),
                ),
            }
        }
    }
}

impl CfgLowerError {
    pub fn add_to_report(self, builder: &mut ReportBuilder<'_>) {
        match self {
            CfgLowerError::UnknownError(e) => {
                builder.set_note(format!("The error message is: {e}"));
            }
            CfgLowerError::UndefinedIdentifier(span) => builder.add_label(
                Label::new((TEST_FILE, span.start..span.end))
                    .with_message("Undefined identifier")
                    .with_color(ariadne::Color::Red),
            ),
            CfgLowerError::UnsupportedExpression(span) => builder.add_label(
                Label::new((TEST_FILE, span.start..span.end))
                    .with_message("Unsupported expression")
                    .with_color(ariadne::Color::Magenta),
            ),
            CfgLowerError::UnsupportedStatement(span) => builder.add_label(
                Label::new((TEST_FILE, span.start..span.end))
                    .with_message("Unsupported statement")
                    .with_color(ariadne::Color::Magenta),
            ),
            CfgLowerError::UnsupportedBinaryOperator(span) => builder.add_label(
                Label::new((TEST_FILE, span.start..span.end))
                    .with_message("Unsupported binary operator")
                    .with_color(ariadne::Color::Magenta),
            ),
        }
    }
}
