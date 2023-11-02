use std::collections::HashMap;

use crate::cfg::{
    return_slot, BasicBlock, BinaryOpKind, CfgBody, Local, LocalKind, Operand, Place, Rvalue,
    Statement, Terminator,
};
use anyhow::bail;
use la_arena::{Arena, Idx};
use lang_c::{
    ast::{self, BlockItem, CallExpression, DeclaratorKind},
    span::Node,
};

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
                    if let Some(rvalue) = self.expr_to_rvalue(&e.node, current)? {
                        self.push_assignment(current, return_slot().into(), rvalue);
                    }
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
                        ast::BlockItem::Declaration(decl) => {
                            let Some(c) = self.lower_declaration(&decl.node, current)? else {
                                return Ok(None);
                            };
                            current = c;
                        }
                        ast::BlockItem::StaticAssert(_) => {
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

    fn lower_declaration(
        &mut self,
        declaration: &ast::Declaration,
        mut current: Idx<BasicBlock>,
    ) -> anyhow::Result<Option<Idx<BasicBlock>>> {
        for declarator in &declaration.declarators {
            let place = match &declarator.node.declarator.node.kind.node {
                DeclaratorKind::Identifier(identifier) => self.named_local(&identifier.node.name),
                DeclaratorKind::Abstract | DeclaratorKind::Declarator(_) => {
                    bail!("not supported: unknown declarator kind")
                }
            };

            if let Some(init) = &declarator.node.initializer {
                match &init.node {
                    ast::Initializer::Expression(expr) => {
                        if let Some(rvalue) = self.expr_to_rvalue(&expr.node, current)? {
                            self.push_assignment(current, place.into(), rvalue);
                        }
                        return Ok(Some(current));
                    }
                    ast::Initializer::List(_) => {
                        bail!("not supported: unknown declarator initializer")
                    }
                }
            }
        }
        Ok(Some(current))
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

    fn push_assignment(&mut self, block: Idx<BasicBlock>, place: Place, rvalue: Rvalue) -> Place {
        self.push_statement(block, Statement::Assign(place.clone(), rvalue));
        place
    }

    fn push_temp_assignment(&mut self, block: Idx<BasicBlock>, rvalue: Rvalue) -> Place {
        let place = self.temp_local().into();
        self.push_assignment(block, place, rvalue)
    }

    fn named_local(&mut self, name: &str) -> Idx<Local> {
        let idx = self
            .result
            .locals
            .alloc(Local::new(LocalKind::Named(name.to_string())));
        self.result.locals.iter_mut().last().unwrap().1.idx = Some(idx);
        self.name_to_local.insert(name.to_string(), idx);
        idx
    }

    fn temp_local(&mut self) -> Idx<Local> {
        let idx = self.result.locals.alloc(Local::new(LocalKind::Anonymous));
        self.result.locals.iter_mut().last().unwrap().1.idx = Some(idx);
        idx
    }

    fn return_local(&mut self) -> Idx<Local> {
        let idx = self.result.locals.alloc(Local::new(LocalKind::ReturnSlot));
        self.result.locals.iter_mut().last().unwrap().1.idx = Some(idx);
        idx
    }

    fn rvalue_to_operand(&mut self, rvalue: Rvalue, bb: Idx<BasicBlock>) -> Operand {
        match rvalue {
            Rvalue::Use(value) => value,
            _ => {
                let temp = self.push_temp_assignment(bb, rvalue);
                Operand::Place(temp)
            }
        }
    }

    fn expr_to_rvalue(
        &mut self,
        expr: &ast::Expression,
        bb: Idx<BasicBlock>,
    ) -> anyhow::Result<Option<Rvalue>> {
        match expr {
            ast::Expression::Identifier(id) => {
                let local = self.resolve_identifier(&id.node)?;
                Ok(Some(Rvalue::Use(Operand::Place(local.into()))))
            }
            ast::Expression::BinaryOperator(bo) => {
                let Some(lhs) = self.expr_to_rvalue(&bo.node.lhs.node, bb)? else {
                        return Ok(None);
                };

                let Some(rhs) = self.expr_to_rvalue(&bo.node.rhs.node, bb)? else {
                    return Ok(None);
                };

                let lhs = self.rvalue_to_operand(lhs, bb);
                let rhs = self.rvalue_to_operand(rhs, bb);

                let rvalue = match bo.node.operator.node {
                    ast::BinaryOperator::Plus => Rvalue::BinaryOp(BinaryOpKind::Add, lhs, rhs),
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
                };

                Ok(Some(rvalue))
            }
            ast::Expression::Constant(constant) => {
                Ok(Some(Rvalue::Use(Operand::Constant(constant.node.clone()))))
            }
            ast::Expression::StringLiteral(_)
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

    fn add_argument_locals(
        &mut self,
        derived: &[Node<ast::DerivedDeclarator>],
    ) -> anyhow::Result<()> {
        self.return_local();
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
                        self.named_local(&arg.node.name);
                    }
                }
                ast::DerivedDeclarator::Pointer(_)
                | ast::DerivedDeclarator::Array(_)
                | ast::DerivedDeclarator::KRFunction(_)
                | ast::DerivedDeclarator::Block(_) => {
                    bail!("invalid declarator type");
                }
            }
        }
        Ok(())
    }
}
