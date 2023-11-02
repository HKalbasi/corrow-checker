use core::panic;

use la_arena::{Arena, Idx, RawIdx};

#[derive(Clone, PartialEq, Eq)]
pub struct Local {
    pub kind: LocalKind,
    pub idx: Option<Idx<Local>>,
}

#[derive(Clone, PartialEq, Eq)]
pub enum LocalKind {
    Anonymous,
    Named(String),
    ReturnSlot,
}

impl std::fmt::Debug for Local {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Some(idx) = self.idx else { return std::fmt::Result::Ok(()); };
        write!(f, "var _{};", idx.into_raw().into_u32())?;

        match &self.kind {
            LocalKind::Named(name) => write!(f, " // {}", name)?,
            LocalKind::Anonymous => (),
            LocalKind::ReturnSlot => write!(f, " // [ReturnSlot]")?,
        }

        std::fmt::Result::Ok(())
    }
}

impl Local {
    pub fn new(kind: LocalKind) -> Self {
        Local { kind, idx: None }
    }
}

pub fn return_slot() -> Idx<Local> {
    Idx::from_raw(RawIdx::from(0))
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Projection {
    Deref,
    Field(String),
    Index(Operand),
}

#[derive(Clone, PartialEq, Eq)]
pub struct Place {
    pub local: Idx<Local>,
    pub projections: Vec<Projection>,
}

impl From<Idx<Local>> for Place {
    fn from(local: Idx<Local>) -> Self {
        Place {
            local,
            projections: vec![],
        }
    }
}

impl std::fmt::Debug for Place {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut dbg = format!("_{}", self.local.into_raw().into_u32());

        for (i, projection) in self.projections.iter().enumerate() {
            dbg = match projection {
                Projection::Deref => format!("&{}", dbg),
                Projection::Field(name) => format!("{}.{}", dbg, name),
                Projection::Index(index) => format!("{}[{:?}]", dbg, index),
            };
        }
        f.write_str(&dbg)
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum Rvalue {
    Use(Operand),
    Ref(Place),
    BinaryOp(BinaryOpKind, Operand, Operand),
}

impl std::fmt::Debug for Rvalue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Use(operand) => write!(f, "{:?}", operand),
            Self::Ref(place) => write!(f, "&{:?}", place),
            Self::BinaryOp(op, lhs, rhs) => write!(f, "{:?} {:?} {:?}", lhs, op, rhs),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum BinaryOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    BitXor,
    BitAnd,
    BitOr,
    Shl,
    Shr,
    Eq,
    Lt,
    Le,
    Ne,
    Ge,
    Gt,
}

impl std::fmt::Debug for BinaryOpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let operator = match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Rem => "%",
            Self::BitXor => "^",
            Self::BitAnd => "&",
            Self::BitOr => "|",
            Self::Shl => "<<",
            Self::Shr => ">>",
            Self::Eq => "==",
            Self::Lt => "<",
            Self::Le => "<=",
            Self::Ne => "!=",
            Self::Ge => ">",
            Self::Gt => ">=",
        };

        f.write_str(operator)
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum Operand {
    Place(Place),
    Constant(ConstOperand),
    External(String),
}

impl std::fmt::Debug for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Place(place) => write!(f, "{:?}", place),
            Self::Constant(constant) => write!(f, "{:?}", constant),
            Self::External(name) => f.write_str(name),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum ConstOperand {
    CConst(lang_c::ast::Constant),
    StringLiteral(Vec<String>),
}

impl std::fmt::Debug for ConstOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Const(")?;

        match self {
            Self::CConst(constant) => {
                let value = match constant {
                    lang_c::ast::Constant::Integer(integer) => format!(
                        "{}{}{}{}{}",
                        match &integer.base {
                            lang_c::ast::IntegerBase::Decimal => "",
                            lang_c::ast::IntegerBase::Octal => "0",
                            lang_c::ast::IntegerBase::Hexadecimal => "0x",
                            lang_c::ast::IntegerBase::Binary => "0b",
                        },
                        integer.number,
                        if integer.suffix.unsigned { "u" } else { "" },
                        match &integer.suffix.size {
                            lang_c::ast::IntegerSize::Int => "",
                            lang_c::ast::IntegerSize::Long => "l",
                            lang_c::ast::IntegerSize::LongLong => "ll",
                        },
                        if integer.suffix.imaginary { "i" } else { "" }
                    ),
                    lang_c::ast::Constant::Float(float) => format!(
                        "{}{}{}{}",
                        match &float.base {
                            lang_c::ast::FloatBase::Decimal => "",
                            lang_c::ast::FloatBase::Hexadecimal => "0x",
                        },
                        float.number,
                        match &float.suffix.format {
                            lang_c::ast::FloatFormat::Double => "",
                            lang_c::ast::FloatFormat::Float => "f",
                            lang_c::ast::FloatFormat::LongDouble => "l",
                            lang_c::ast::FloatFormat::TS18661Format(_) => "df",
                        },
                        if float.suffix.imaginary { "i" } else { "" }
                    ),
                    lang_c::ast::Constant::Character(string) => format!("{}", string),
                };
                write!(f, "{}", value)?;
            }
            Self::StringLiteral(literals) => write!(f, "{}", literals.join(" "))?,
        }

        f.write_str(")")
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum Statement {
    Assign(Place, Rvalue),
}

impl std::fmt::Debug for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Assign(lhs, rhs) => write!(f, "{:#?} = {:#?}", lhs, rhs),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum Terminator {
    Return,
    Call(Operand, Vec<Operand>, Place, Idx<BasicBlock>),
    Goto(Idx<BasicBlock>),
    SwitchInt(Operand, Vec<u128>, Vec<Idx<BasicBlock>>),
}

impl std::fmt::Debug for Terminator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Terminator::Return => f.write_str("return"),
            Terminator::Call(op, args, place, block) => {
                write!(f, "{:?} = {:?}(", place, op)?;
                for (i, arg) in args.iter().enumerate() {
                    write!(f, "{:?}", arg)?;
                    if i != args.len() - 1 {
                        f.write_str(", ")?;
                    }
                }
                write!(f, ") -> 'bb{}", block.into_raw().into_u32())
            }
            Terminator::Goto(bb) => write!(f, "goto 'bb{}", bb.into_raw().into_u32()),
            Terminator::SwitchInt(op, arms, targets) => {
                if arms.len() + 1 != targets.len() {
                    panic!("This should not be possible");
                }
                writeln!(f, "switch {:?} {{", op)?;
                for (i, arm) in arms.iter().enumerate() {
                    writeln!(
                        f,
                        "    {} => 'bb{},",
                        arm,
                        targets.get(i).unwrap().into_raw().into_u32()
                    )?;
                }
                writeln!(
                    f,
                    "    _ => 'bb{},",
                    targets.last().unwrap().into_raw().into_u32()
                )?;
                f.write_str("}")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct BasicBlock {
    /// List of statements in this block.
    pub statements: Vec<Statement>,

    /// Terminator for this block.
    ///
    /// N.B., this should generally ONLY be `None` during construction.
    /// Therefore, you should generally access it via the
    /// `terminator()` or `terminator_mut()` methods. The only
    /// exception is that certain passes, such as `simplify_cfg`, swap
    /// out the terminator temporarily with `None` while they continue
    /// to recurse over the set of basic blocks.
    pub terminator: Option<Terminator>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CfgBody {
    pub basic_blocks: Arena<BasicBlock>,
    pub locals: Arena<Local>,
}
