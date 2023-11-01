use la_arena::{Arena, Idx, RawIdx};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Local;

pub fn return_slot() -> Idx<Local> {
    Idx::from_raw(RawIdx::from(0))
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Projection {
    Deref,
    Field(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Rvalue {
    Use(Place),
    Ref(Place),
    Plus(Place, Place),
    Constant(lang_c::ast::Constant),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    Assign(Place, Rvalue),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Terminator {
    Return,
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
