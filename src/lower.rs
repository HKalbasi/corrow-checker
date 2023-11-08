use std::collections::HashMap;

use crate::cfg::{
    return_slot, BasicBlock, BinaryOpKind, CfgBody, CfgStatics, ConstOperand, Local, LocalKind,
    Operand, Ownership, Place, RawPlace, Rvalue, Statement, Static, Terminator,
};
use anyhow::bail;
use la_arena::{Arena, Idx};
use lang_c::{
    ast::{self, DeclaratorKind, IntegerSuffix, PointerQualifier},
    span::{Node, Span},
};

struct ActiveBb {
    idx: Idx<BasicBlock>,
    continue_bb: Option<Idx<BasicBlock>>,
    break_bb: Option<Idx<BasicBlock>>,
}

impl ActiveBb {
    fn new(idx: Idx<BasicBlock>) -> Self {
        ActiveBb {
            idx,
            continue_bb: None,
            break_bb: None,
        }
    }

    fn switch(&mut self, new_idx: Idx<BasicBlock>) {
        self.idx = new_idx;
    }

    fn idx(&self) -> Idx<BasicBlock> {
        self.idx.clone()
    }

    fn activate_loop(&mut self, continue_bb: Idx<BasicBlock>, break_bb: Idx<BasicBlock>) {
        self.continue_bb = Some(continue_bb);
        self.break_bb = Some(break_bb);
    }

    fn deactivate_loop(&mut self) {
        self.continue_bb = None;
        self.break_bb = None;
    }
}

pub fn lower_statics(decl: &ast::Declaration, statics: &mut CfgStatics) -> anyhow::Result<()> {
    for decls in &decl.declarators {
        if let DeclaratorKind::Identifier(id) = &decls.node.declarator.node.kind.node {
            let name = id.node.name.clone();
            let mut ret_ownership = None;
            let mut param_ownerships = vec![];
            let mut is_function = false;

            for derived_decl in &decls.node.declarator.node.derived {
                match &derived_decl.node {
                    lang_c::ast::DerivedDeclarator::Pointer(_) => {
                        ret_ownership =
                            extract_ownership_from_derived_declarator(&derived_decl.node)?;
                    }
                    lang_c::ast::DerivedDeclarator::Function(fnd) => {
                        is_function = true;
                        for param in &fnd.node.parameters {
                            let param_ownership = match &param.node.declarator {
                                Some(pdecl) => extract_ownership_from_declarator(&pdecl.node)?,
                                None => None,
                            };
                            param_ownerships.push(param_ownership);
                        }
                    }
                    _ => (),
                }
            }

            let sttc = if is_function {
                Static::Function(ret_ownership, param_ownerships)
            } else {
                Static::Variable(ret_ownership)
            };

            let idx = statics.statics.alloc(sttc.into());
            statics.name_to_static.insert(name, idx);
        }
    }

    Ok(())
}

pub fn lower_body<'a>(
    name: String,
    fd: &ast::FunctionDefinition,
    statics: &'a CfgStatics,
) -> anyhow::Result<CfgBody<'a>> {
    let mut ctx = LowerCtx::new(name, statics);
    ctx.add_argument_locals(&fd.declarator.node.derived);
    let start = ctx.new_basic_block();
    let mut active_bb = ActiveBb::new(start);
    ctx.lower_statement(&fd.statement, &mut active_bb)?;
    Ok(ctx.result)
}

struct LowerCtx<'a> {
    result: CfgBody<'a>,
}

impl<'a> LowerCtx<'a> {
    fn new(name: String, statics: &CfgStatics) -> LowerCtx {
        LowerCtx {
            result: CfgBody {
                basic_blocks: Arena::new(),
                locals: Arena::new(),
                name,
                statics,
                name_to_local: HashMap::new(),
            },
        }
    }

    fn new_basic_block(&mut self) -> Idx<BasicBlock> {
        self.result.basic_blocks.alloc(BasicBlock::default())
    }

    fn set_terminator(&mut self, terminator: Terminator, bb_idx: Idx<BasicBlock>) {
        let bb = &mut self.result.basic_blocks[bb_idx];
        if let Some(_) = bb.terminator {
            dbg!("-- resetting terminator ignored --");
        } else {
            bb.terminator = Some(terminator);
        }
    }

    fn lower_statement(
        &mut self,
        statement: &Node<ast::Statement>,
        active_bb: &mut ActiveBb,
    ) -> anyhow::Result<()> {
        match &statement.node {
            ast::Statement::Return(e) => {
                if let Some(e) = e {
                    let rvalue = self.expr_to_rvalue(&e.node, active_bb)?;
                    self.push_assignment(return_slot().into(), rvalue, active_bb, e.span);
                }
                self.set_terminator(Terminator::Return(statement.span), active_bb.idx);
                Ok(())
            }
            ast::Statement::Compound(stmts) => {
                for stmt in stmts {
                    match &stmt.node {
                        ast::BlockItem::Statement(stmt) => {
                            self.lower_statement(&stmt, active_bb)?;
                        }
                        ast::BlockItem::Declaration(decl) => {
                            self.lower_declaration(&decl.node, active_bb)?;
                        }
                        ast::BlockItem::StaticAssert(_) => {
                            bail!("not supported: unknown block item");
                        }
                    }
                }
                Ok(())
            }
            ast::Statement::Expression(expr) => {
                let Some(expr) = expr else { return Ok(()) };
                let old_bb = active_bb.idx;
                let rvalue = self.expr_to_rvalue(&expr.node, active_bb)?;
                self.push_discard_if_required(rvalue, active_bb, expr.span);
                Ok(())
            }
            ast::Statement::While(whl) => {
                let continue_bb = self.new_basic_block();
                self.set_terminator(Terminator::Goto(continue_bb), active_bb.idx);
                active_bb.switch(continue_bb);
                let operand = self.expr_to_operand(&whl.node.expression, active_bb)?;
                let loop_bb = self.new_basic_block();
                let break_bb = self.new_basic_block();
                let terminator = Terminator::SwitchInt(
                    operand,
                    vec![1],
                    vec![loop_bb.clone(), break_bb.clone()],
                );
                self.set_terminator(terminator, active_bb.idx);
                active_bb.switch(loop_bb);
                active_bb.activate_loop(continue_bb, break_bb);
                self.lower_statement(&whl.node.statement, active_bb)?;
                self.set_terminator(Terminator::Goto(continue_bb), active_bb.idx);
                active_bb.deactivate_loop();
                active_bb.switch(break_bb);
                Ok(())
            }
            ast::Statement::For(fr) => {
                match fr.node.initializer.node {
                    ast::ForInitializer::Empty => (),
                    _ => {
                        let init_bb = self.new_basic_block();
                        self.set_terminator(Terminator::Goto(init_bb), active_bb.idx);
                        active_bb.switch(init_bb);
                        match &fr.node.initializer.node {
                            ast::ForInitializer::Empty => unreachable!(),
                            ast::ForInitializer::Expression(expr) => {
                                let rvalue = self.expr_to_rvalue(&expr.node, active_bb)?;
                                self.push_discard_if_required(rvalue, active_bb, fr.span);
                            }
                            ast::ForInitializer::Declaration(decl) => {
                                self.lower_declaration(&decl.node, active_bb)?;
                            }
                            ast::ForInitializer::StaticAssert(_) => {
                                bail!("not supported: StaticAssert is not supported")
                            }
                        }
                    }
                }

                // The following two statements help allocate condition and step bbs before loop and break bbs
                let condition_info = fr
                    .node
                    .condition
                    .as_ref()
                    .map(|condition| (condition, self.new_basic_block()));

                let step_info = fr
                    .node
                    .step
                    .as_ref()
                    .map(|step| (step, self.new_basic_block()));

                let loop_bb = self.new_basic_block();
                let break_bb = self.new_basic_block();
                let mut continue_bb = loop_bb;

                if let Some((condition, bb)) = condition_info {
                    continue_bb = bb;
                    self.set_terminator(Terminator::Goto(continue_bb), active_bb.idx);
                    active_bb.switch(continue_bb);
                    let operand = self.expr_to_operand(&condition, active_bb)?;

                    let terminator = Terminator::SwitchInt(
                        operand,
                        vec![1],
                        vec![loop_bb.clone(), break_bb.clone()],
                    );
                    self.set_terminator(terminator, active_bb.idx);
                }

                if let Some((step, bb)) = step_info {
                    let step_bb = bb;
                    active_bb.switch(step_bb);
                    let rvalue = self.expr_to_rvalue(&step.node, active_bb)?;
                    self.push_discard_if_required(rvalue, active_bb, fr.span);
                    self.set_terminator(Terminator::Goto(continue_bb), active_bb.idx);
                    continue_bb = step_bb;
                }

                active_bb.switch(loop_bb);
                active_bb.activate_loop(continue_bb, break_bb);
                self.lower_statement(&fr.node.statement, active_bb)?;
                self.set_terminator(Terminator::Goto(continue_bb), active_bb.idx);
                active_bb.deactivate_loop();
                active_bb.switch(break_bb);
                Ok(())
            }
            ast::Statement::Continue => {
                let Some(continue_bb) = active_bb.continue_bb else { bail!("unexpected statement: continue is not used in a loop") };
                self.set_terminator(Terminator::Goto(continue_bb), active_bb.idx);
                Ok(())
            }
            ast::Statement::Break => {
                let Some(break_bb) = active_bb.break_bb else { bail!("unexpected statement: break is not used in a loop") };
                self.set_terminator(Terminator::Goto(break_bb), active_bb.idx);
                Ok(())
            }
            ast::Statement::If(eef) => self.lower_if_statement(&eef.node, active_bb),
            ast::Statement::Labeled(_)
            | ast::Statement::Switch(_)
            | ast::Statement::DoWhile(_)
            | ast::Statement::Goto(_)
            | ast::Statement::Asm(_) => bail!("not supported: unknown statement type"),
        }
    }

    fn push_discard_if_required(&mut self, rvalue: Rvalue, active_bb: &mut ActiveBb, span: Span) {
        match rvalue {
            Rvalue::Use(_) => (),
            _ => {
                self.push_temp_assignment(rvalue, active_bb, span);
            }
        }
    }

    fn lower_if_statement(
        &mut self,
        statement: &ast::IfStatement,
        active_bb: &mut ActiveBb,
    ) -> anyhow::Result<()> {
        let mut branches = vec![(
            statement,
            active_bb.idx, /* the first if does not require a seperate condition_bb */
            self.new_basic_block(),
        )];
        let mut it = statement;
        let mut uncoditional_else = None;
        while let Some(next) = &it.else_statement {
            if let lang_c::ast::Statement::If(next_eef) = &next.node {
                it = &next_eef.node;
                // The first if does not require a separate condition_bb
                let condition_bb = self.new_basic_block();
                let then_bb = self.new_basic_block();
                branches.push((&next_eef.node, condition_bb, then_bb));
            } else {
                uncoditional_else = Some((&*next, self.new_basic_block()));
                break;
            }
        }

        let after_bb = self.new_basic_block();

        for (i, (eef, condition_bb, then_bb)) in branches.iter().enumerate() {
            active_bb.switch(condition_bb.clone());
            let condition = self.expr_to_operand(&eef.condition, active_bb)?;
            let nottaken_bb = match branches.get(i + 1) {
                Some((_, next_condition_bb, _)) => next_condition_bb.clone(),
                None => match uncoditional_else {
                    Some((_, uncoditional_bb)) => uncoditional_bb.clone(),
                    None => after_bb.clone(),
                },
            };

            self.set_terminator(
                Terminator::SwitchInt(condition, vec![1], vec![then_bb.clone(), nottaken_bb]),
                active_bb.idx,
            );
            active_bb.switch(then_bb.clone());
            self.lower_statement(&eef.then_statement, active_bb)?;
            self.set_terminator(Terminator::Goto(after_bb), active_bb.idx);
        }

        if let Some((else_statement, else_bb)) = uncoditional_else {
            active_bb.switch(else_bb.clone());
            self.lower_statement(&else_statement, active_bb)?;
            self.set_terminator(Terminator::Goto(after_bb), active_bb.idx);
        }

        active_bb.switch(after_bb);
        Ok(())
    }

    fn lower_declaration(
        &mut self,
        declaration: &ast::Declaration,
        active_bb: &mut ActiveBb,
    ) -> anyhow::Result<()> {
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
                        let rvalue = self.expr_to_rvalue(&expr.node, active_bb)?;
                        self.push_assignment(place.into(), rvalue, active_bb, init.span);
                        return Ok(());
                    }
                    ast::Initializer::List(_) => {
                        bail!("not supported: unknown declarator initializer")
                    }
                }
            }
        }
        Ok(())
    }

    fn resolve_identifier(&self, id: &ast::Identifier) -> anyhow::Result<Idx<Local>> {
        match self.result.name_to_local.get(&id.name) {
            Some(x) => Ok(*x),
            None => bail!("unresolved name {}", id.name),
        }
    }

    fn resolve_identifier_static(&self, id: &ast::Identifier) -> anyhow::Result<Idx<Static>> {
        match self.result.statics.name_to_static.get(&id.name) {
            Some(x) => Ok(*x),
            None => bail!("unresolved name {}", id.name),
        }
    }

    fn push_statement(&mut self, statement: Statement, active_bb: &mut ActiveBb) {
        let bb = &mut self.result.basic_blocks[active_bb.idx];

        if let Some(_) = bb.terminator {
            dbg!("-- pushing statement ignore since the block has a terminator --");
        } else {
            bb.statements.push(statement);
        }
    }

    fn push_assignment(
        &mut self,
        place: Place,
        rvalue: Rvalue,
        active_bb: &mut ActiveBb,
        span: Span,
    ) -> Place {
        self.push_statement(Statement::Assign(place.clone(), rvalue, span), active_bb);
        place
    }

    fn push_temp_assignment(
        &mut self,
        rvalue: Rvalue,
        active_bb: &mut ActiveBb,
        span: Span,
    ) -> Place {
        let place = self.temp_local().into();
        self.push_assignment(place, rvalue, active_bb, span)
    }

    fn named_local(&mut self, name: &str) -> Idx<Local> {
        let idx = self
            .result
            .locals
            .alloc(Local::new(LocalKind::Named(name.to_string()), None));
        self.result.locals.iter_mut().last().unwrap().1.idx = Some(idx);
        self.result.name_to_local.insert(name.to_string(), idx);
        idx
    }

    fn temp_local(&mut self) -> Idx<Local> {
        let idx = self
            .result
            .locals
            .alloc(Local::new(LocalKind::Anonymous, None));
        self.result.locals.iter_mut().last().unwrap().1.idx = Some(idx);
        idx
    }

    fn return_local(&mut self) -> Idx<Local> {
        let idx = self
            .result
            .locals
            .alloc(Local::new(LocalKind::ReturnSlot, None));
        self.result.locals.iter_mut().last().unwrap().1.idx = Some(idx);
        idx
    }

    fn rvalue_to_operand(
        &mut self,
        rvalue: Rvalue,
        active_bb: &mut ActiveBb,
        span: Span,
    ) -> Operand {
        match rvalue {
            Rvalue::Use(value) => value,
            _ => {
                let temp = self.push_temp_assignment(rvalue, active_bb, span);
                Operand::Place(temp)
            }
        }
    }

    fn rvalue_to_operand_place(
        &mut self,
        rvalue: Rvalue,
        active_bb: &mut ActiveBb,
        span: Span,
    ) -> Place {
        match rvalue {
            Rvalue::Use(Operand::Place(place)) => place,
            _ => self.push_temp_assignment(rvalue, active_bb, span),
        }
    }

    fn expr_to_rvalue(
        &mut self,
        expr: &ast::Expression,
        active_bb: &mut ActiveBb,
    ) -> anyhow::Result<Rvalue> {
        match expr {
            ast::Expression::Identifier(id) => {
                let operand = match self.resolve_identifier(&id.node) {
                    Ok(local) => Operand::Place(local.into()),
                    Err(_) => match self.resolve_identifier_static(&id.node) {
                        Ok(sttc) => Operand::Place(sttc.into()),
                        Err(_) => bail!("Undefined identifier {}", &id.node.name),
                    },
                };
                Ok(Rvalue::Use(operand))
            }
            ast::Expression::BinaryOperator(bo) => {
                let rvalue = match bo.node.operator.node {
                    ast::BinaryOperator::Index => {
                        let mut lhs = self.expr_to_operand_place(&bo.node.lhs, active_bb)?;
                        let rhs = self.expr_to_operand(&bo.node.rhs, active_bb)?;
                        lhs.projections.push(crate::cfg::Projection::Index(rhs));
                        Rvalue::Use(Operand::Place(lhs))
                    }
                    ast::BinaryOperator::Assign => {
                        let lhs = self.expr_to_operand_place(&bo.node.lhs, active_bb)?;
                        let rhs = self.expr_to_rvalue(&bo.node.rhs.node, active_bb)?;
                        let place = self.push_assignment(lhs, rhs, active_bb, bo.span);
                        Rvalue::Use(Operand::Place(place))
                    }
                    ast::BinaryOperator::LogicalAnd
                    | ast::BinaryOperator::LogicalOr
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
                    _ => {
                        let lhs = self.expr_to_operand(&bo.node.lhs, active_bb)?;
                        let rhs = self.expr_to_operand(&bo.node.rhs, active_bb)?;
                        let op_kind = match bo.node.operator.node {
                            ast::BinaryOperator::Plus => BinaryOpKind::Add,
                            ast::BinaryOperator::Minus => BinaryOpKind::Sub,
                            ast::BinaryOperator::Less => BinaryOpKind::Lt,
                            ast::BinaryOperator::Greater => BinaryOpKind::Gt,
                            ast::BinaryOperator::LessOrEqual => BinaryOpKind::Le,
                            ast::BinaryOperator::GreaterOrEqual => BinaryOpKind::Ge,
                            ast::BinaryOperator::Equals => BinaryOpKind::Eq,
                            ast::BinaryOperator::NotEquals => BinaryOpKind::Ne,
                            ast::BinaryOperator::Index => unreachable!(),
                            ast::BinaryOperator::Multiply => BinaryOpKind::Mul,
                            ast::BinaryOperator::Divide => BinaryOpKind::Div,
                            ast::BinaryOperator::Modulo => BinaryOpKind::Rem,
                            ast::BinaryOperator::ShiftLeft => BinaryOpKind::Shl,
                            ast::BinaryOperator::ShiftRight => BinaryOpKind::Shr,
                            ast::BinaryOperator::BitwiseAnd => BinaryOpKind::BitAnd,
                            ast::BinaryOperator::BitwiseXor => BinaryOpKind::BitXor,
                            ast::BinaryOperator::BitwiseOr => BinaryOpKind::BitOr,
                            _ => bail!("not supported: unknown binary operator"),
                        };
                        Rvalue::BinaryOp(op_kind, lhs, rhs)
                    }
                };

                Ok(rvalue)
            }
            ast::Expression::Constant(constant) => Ok(Rvalue::Use(Operand::Constant(
                ConstOperand::CConst(constant.node.clone()),
            ))),
            ast::Expression::StringLiteral(literals) => Ok(Rvalue::Use(Operand::Constant(
                ConstOperand::StringLiteral(literals.node.clone()),
            ))),
            ast::Expression::UnaryOperator(unary) => match &unary.node.operator.node {
                ast::UnaryOperator::Address => {
                    let operand = self.expr_to_operand_place(&unary.node.operand, active_bb)?;
                    Ok(Rvalue::Ref(operand))
                }
                ast::UnaryOperator::PostIncrement => {
                    let operand = self.expr_to_operand_place(&unary.node.operand, active_bb)?;
                    let place = self.push_assignment(
                        operand.clone(),
                        Rvalue::BinaryOp(
                            BinaryOpKind::Add,
                            Operand::Place(operand.clone()),
                            Self::integer_operand("1"),
                        ),
                        active_bb,
                        unary.span,
                    );
                    Ok(Rvalue::Use(Operand::Place(place)))
                }
                ast::UnaryOperator::PostDecrement => {
                    let operand = self.expr_to_operand_place(&unary.node.operand, active_bb)?;
                    let place = self.push_assignment(
                        operand.clone(),
                        Rvalue::BinaryOp(
                            BinaryOpKind::Sub,
                            Operand::Place(operand.clone()),
                            Self::integer_operand("1"),
                        ),
                        active_bb,
                        unary.span,
                    );
                    Ok(Rvalue::Use(Operand::Place(place)))
                }
                ast::UnaryOperator::PreIncrement => {
                    let operand = self.expr_to_operand_place(&unary.node.operand, active_bb)?;
                    let place = self.push_temp_assignment(
                        Rvalue::Use(Operand::Place(operand.clone())),
                        active_bb,
                        unary.span,
                    );
                    self.push_assignment(
                        operand.clone(),
                        Rvalue::BinaryOp(
                            BinaryOpKind::Add,
                            Operand::Place(operand.clone()),
                            Self::integer_operand("1"),
                        ),
                        active_bb,
                        unary.span,
                    );
                    Ok(Rvalue::Use(Operand::Place(place)))
                }
                ast::UnaryOperator::PreDecrement => {
                    let operand = self.expr_to_operand_place(&unary.node.operand, active_bb)?;
                    let place = self.push_temp_assignment(
                        Rvalue::Use(Operand::Place(operand.clone())),
                        active_bb,
                        unary.span,
                    );
                    self.push_assignment(
                        operand.clone(),
                        Rvalue::BinaryOp(
                            BinaryOpKind::Sub,
                            Operand::Place(operand.clone()),
                            Self::integer_operand("1"),
                        ),
                        active_bb,
                        unary.span,
                    );
                    Ok(Rvalue::Use(Operand::Place(place)))
                }
                ast::UnaryOperator::Indirection => todo!(),
                ast::UnaryOperator::Plus => todo!(),
                ast::UnaryOperator::Minus => todo!(),
                ast::UnaryOperator::Complement => todo!(),
                ast::UnaryOperator::Negate => todo!(),
            },
            ast::Expression::Call(call) => {
                let callee = self.expr_to_operand(&call.node.callee, active_bb)?;
                let args = call
                    .node
                    .arguments
                    .iter()
                    .map(|x| self.expr_to_operand(&x, active_bb))
                    .collect::<anyhow::Result<Vec<Operand>>>()?;
                let destination = self.temp_local();
                let new_bb = self.new_basic_block();
                self.set_terminator(
                    Terminator::Call {
                        callee,
                        args,
                        return_place: destination.into(),
                        target: new_bb,
                        span: call.span,
                    },
                    active_bb.idx,
                );
                active_bb.switch(new_bb);
                Ok(Rvalue::Use(Operand::Place(destination.into())))
            }
            ast::Expression::GenericSelection(_)
            | ast::Expression::Member(_)
            | ast::Expression::CompoundLiteral(_)
            | ast::Expression::SizeOfTy(_)
            | ast::Expression::SizeOfVal(_)
            | ast::Expression::AlignOf(_)
            | ast::Expression::Cast(_)
            | ast::Expression::Conditional(_)
            | ast::Expression::Comma(_)
            | ast::Expression::OffsetOf(_)
            | ast::Expression::VaArg(_)
            | ast::Expression::Statement(_) => bail!("not supported: unknown expression"),
        }
    }

    fn expr_to_operand(
        &mut self,
        expr: &Node<ast::Expression>,
        active_bb: &mut ActiveBb,
    ) -> anyhow::Result<Operand> {
        let rvalue = self.expr_to_rvalue(&expr.node, active_bb)?;
        Ok(self.rvalue_to_operand(rvalue, active_bb, expr.span))
    }

    fn expr_to_operand_place(
        &mut self,
        expr: &Node<ast::Expression>,
        active_bb: &mut ActiveBb,
    ) -> anyhow::Result<Place> {
        let rvalue = self.expr_to_rvalue(&expr.node, active_bb)?;
        Ok(self.rvalue_to_operand_place(rvalue, active_bb, expr.span))
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

    pub fn integer_operand(number: &str) -> Operand {
        Operand::Constant(ConstOperand::CConst(ast::Constant::Integer(ast::Integer {
            base: ast::IntegerBase::Decimal,
            number: number.into(),
            suffix: IntegerSuffix {
                size: ast::IntegerSize::Int,
                unsigned: false,
                imaginary: false,
            },
        })))
    }
}

fn extract_ownership_from_declarator(decl: &ast::Declarator) -> anyhow::Result<Option<Ownership>> {
    let mut ownership = None;

    for derived in &decl.derived {
        let temp = extract_ownership_from_derived_declarator(&derived.node)?;

        if temp.is_some() {
            if ownership.is_some() {
                bail!("Multiple ownership qualifiers are not allowed");
            }
        }

        ownership = temp;
    }

    Ok(ownership)
}

fn extract_ownership_from_derived_declarator(
    decl: &ast::DerivedDeclarator,
) -> anyhow::Result<Option<Ownership>> {
    let mut ownership = None;
    match decl {
        ast::DerivedDeclarator::Pointer(pqs) => {
            for pq in pqs {
                let temp = extract_ownership_from_pointer_qualifier(&pq.node);

                if temp.is_some() {
                    if ownership.is_some() {
                        bail!("Multiple ownership qualifiers are not allowed");
                    }

                    ownership = temp;
                }
            }
        }
        _ => (),
    };

    Ok(ownership)
}

fn extract_ownership_from_pointer_qualifier(
    qualifier: &ast::PointerQualifier,
) -> Option<Ownership> {
    if let PointerQualifier::TypeQualifier(type_qualifier) = qualifier {
        match type_qualifier.node {
            ast::TypeQualifier::Owned => Some(Ownership::Owned),
            ast::TypeQualifier::Borrowed => Some(Ownership::Borrowed),
            ast::TypeQualifier::BorrowedMut => Some(Ownership::BorrowedMut),
            _ => None,
        }
    } else {
        None
    }
}
