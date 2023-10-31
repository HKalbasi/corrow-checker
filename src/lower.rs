use std::collections::HashMap;

use crate::cfg::{return_slot, BasicBlock, CfgBody, Local, Place, Rvalue, Statement, Terminator};
use anyhow::bail;
use la_arena::{Arena, Idx};
use lang_c::{ast::{self, DeclaratorKind}, span::Node};

pub fn lower_body(fd: &ast::FunctionDefinition) -> anyhow::Result<CfgBody> {
    let mut ctx = LowerCtx::new();
    ctx.add_argument_locals(&fd.declarator.node.derived);
    let start = ctx.new_basic_block();
    ctx.lower_statement(&fd.statement.node, start)?;
    Ok(ctx.result)
}

struct LowerCtx {
    result: CfgBody,
    name_to_local: HashMap<String, Idx<Local>>,
}

impl LowerCtx {
    fn new() -> LowerCtx {
        LowerCtx {
            result: CfgBody {
                basic_blocks: Arena::new(),
                locals: Arena::new(),
            },
            name_to_local: HashMap::new(),
        }
    }

    fn new_basic_block(&mut self) -> Idx<BasicBlock> {
        self.result.basic_blocks.alloc(BasicBlock::default())
    }

    fn set_terminator(&mut self, source: Idx<BasicBlock>, terminator: Terminator) {
        self.result.basic_blocks[source].terminator = Some(terminator);
    }

    fn lower_statement(
        &mut self,
        statement: &ast::Statement,
        mut current: Idx<BasicBlock>,
    ) -> anyhow::Result<Option<Idx<BasicBlock>>> {
        match statement {
            ast::Statement::Return(e) => {
                if let Some(e) = e {
                    self.lower_expr_to_place(&e.node, return_slot().into(), current)?;
                }
                self.set_terminator(current, Terminator::Return);
                Ok(None)
            }
            ast::Statement::Compound(stmts) => {
                for stmt in stmts {
                    match &stmt.node {
                        ast::BlockItem::Statement(stmt) => {
                            let Some(c) = self.lower_statement(&stmt.node, current)? else {
                                return Ok(None);
                            };
                            current = c;
                        }
                        ast::BlockItem::Declaration(_) | ast::BlockItem::StaticAssert(_) => {
                            bail!("not supported: unknown block item");
                        }
                    }
                }
                Ok(Some(current))
            }
            ast::Statement::Labeled(_)
            | ast::Statement::Expression(_)
            | ast::Statement::If(_)
            | ast::Statement::Switch(_)
            | ast::Statement::While(_)
            | ast::Statement::DoWhile(_)
            | ast::Statement::For(_)
            | ast::Statement::Goto(_)
            | ast::Statement::Continue
            | ast::Statement::Break
            | ast::Statement::Asm(_) => bail!("not supported: unknown statement type"),
        }
    }

    fn resolve_identifier(&self, id: &ast::Identifier) -> anyhow::Result<Idx<Local>> {
        match self.name_to_local.get(&id.name) {
            Some(x) => Ok(*x),
            None => bail!("unresolved name {}", id.name),
        }
    }

    fn push_statement(&mut self, block: Idx<BasicBlock>, statement: Statement) {
        self.result.basic_blocks[block].statements.push(statement);
    }

    fn push_assignment(&mut self, block: Idx<BasicBlock>, place: Place, rvalue: Rvalue) {
        self.push_statement(block, Statement::Assign(place, rvalue));
    }

    fn temp(&mut self) -> Idx<Local> {
        self.result.locals.alloc(Local)
    }

    fn lower_expr_to_some_place(
        &mut self,
        expr: &ast::Expression,
        current: Idx<BasicBlock>,
    ) -> anyhow::Result<Option<(Place, Idx<BasicBlock>)>> {
        let place: Place = self.temp().into();
        let Some(bb) = self.lower_expr_to_place(expr, place.clone(), current)? else {
            return Ok(None);
        };
        Ok(Some((place, bb)))
    }

    fn lower_expr_to_place(
        &mut self,
        expr: &ast::Expression,
        place: Place,
        mut current: Idx<BasicBlock>,
    ) -> anyhow::Result<Option<Idx<BasicBlock>>> {
        match expr {
            ast::Expression::Identifier(id) => {
                let local = self.resolve_identifier(&id.node)?;
                self.push_assignment(current, place, Rvalue::Use(local.into()));
                Ok(Some(current))
            }
            ast::Expression::BinaryOperator(bo) => {
                let Some((lhs, c)) = self.lower_expr_to_some_place(&bo.node.lhs.node, current)?
                else {
                    return Ok(None);
                };
                current = c;
                let Some((rhs, c)) = self.lower_expr_to_some_place(&bo.node.rhs.node, current)?
                else {
                    return Ok(None);
                };
                current = c;
                match bo.node.operator.node {
                    ast::BinaryOperator::Plus => {
                        self.push_assignment(current, place, Rvalue::Plus(lhs, rhs));
                    }
                    ast::BinaryOperator::Index
                    | ast::BinaryOperator::Multiply
                    | ast::BinaryOperator::Divide
                    | ast::BinaryOperator::Modulo
                    | ast::BinaryOperator::Minus
                    | ast::BinaryOperator::ShiftLeft
                    | ast::BinaryOperator::ShiftRight
                    | ast::BinaryOperator::Less
                    | ast::BinaryOperator::Greater
                    | ast::BinaryOperator::LessOrEqual
                    | ast::BinaryOperator::GreaterOrEqual
                    | ast::BinaryOperator::Equals
                    | ast::BinaryOperator::NotEquals
                    | ast::BinaryOperator::BitwiseAnd
                    | ast::BinaryOperator::BitwiseXor
                    | ast::BinaryOperator::BitwiseOr
                    | ast::BinaryOperator::LogicalAnd
                    | ast::BinaryOperator::LogicalOr
                    | ast::BinaryOperator::Assign
                    | ast::BinaryOperator::AssignMultiply
                    | ast::BinaryOperator::AssignDivide
                    | ast::BinaryOperator::AssignModulo
                    | ast::BinaryOperator::AssignPlus
                    | ast::BinaryOperator::AssignMinus
                    | ast::BinaryOperator::AssignShiftLeft
                    | ast::BinaryOperator::AssignShiftRight
                    | ast::BinaryOperator::AssignBitwiseAnd
                    | ast::BinaryOperator::AssignBitwiseXor
                    | ast::BinaryOperator::AssignBitwiseOr => {
                        bail!("not supported: unknown binary operator")
                    }
                }
                Ok(Some(current))
            }
            ast::Expression::Constant(_)
            | ast::Expression::StringLiteral(_)
            | ast::Expression::GenericSelection(_)
            | ast::Expression::Member(_)
            | ast::Expression::Call(_)
            | ast::Expression::CompoundLiteral(_)
            | ast::Expression::SizeOfTy(_)
            | ast::Expression::SizeOfVal(_)
            | ast::Expression::AlignOf(_)
            | ast::Expression::UnaryOperator(_)
            | ast::Expression::Cast(_)
            | ast::Expression::Conditional(_)
            | ast::Expression::Comma(_)
            | ast::Expression::OffsetOf(_)
            | ast::Expression::VaArg(_)
            | ast::Expression::Statement(_) => bail!("not supported: unknown expression"),
        }
    }

    fn add_argument_locals(&mut self, derived: &[Node<ast::DerivedDeclarator>]) -> anyhow::Result<()> {
        self.result.locals.alloc(Local); // Return slot
        for arg in derived {
            match &arg.node {
                ast::DerivedDeclarator::Function(arg) => {
                    for arg in &arg.node.parameters {
                        let Some(arg) = &arg.node.declarator else {
                            bail!("not supported: parameter without name");
                        };
                        let DeclaratorKind::Identifier(arg) = &arg.node.kind.node else {
                            bail!("invalid declarator kind");
                        };
                        let name = &arg.node.name;
                        let local_id = self.result.locals.alloc(Local);
                        self.name_to_local.insert(name.clone(), local_id);
                    }

                },
                ast::DerivedDeclarator::Pointer(_)
                | ast::DerivedDeclarator::Array(_)
                | ast::DerivedDeclarator::KRFunction(_)
                | ast::DerivedDeclarator::Block(_) => {
                    bail!("invalid declarator type");
                },
            }
        }
        Ok(())
    }
}
