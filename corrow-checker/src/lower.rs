use std::collections::HashMap;

use crate::cfg::{
    return_slot, BasicBlock, BinaryOpKind, CfgBody, CfgStatics, ConstOperand, Local, LocalKind,
    Operand, Ownership, Place, Rvalue, Statement, Static, Terminator,
};
use anyhow::{anyhow, bail};
use la_arena::{Arena, Idx};
use lang_c::{
    ast::{self, DeclaratorKind, IntegerSuffix, PointerQualifier},
    span::{Node, Span},
};

pub enum CfgLowerError {
    UndefinedIdentifier(Span),
    UnsupportedExpression(Span),
    UnsupportedStatement(Span),
    UnsupportedBinaryOperator(Span),
    UnsupportedUnaryOperator(Span),
    UnknownError(anyhow::Error),
}

impl From<anyhow::Error> for CfgLowerError {
    fn from(value: anyhow::Error) -> Self {
        Self::UnknownError(value)
    }
}

type Result<T> = std::result::Result<T, CfgLowerError>;

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

impl CfgStatics {
    pub fn lower_statics(&mut self, decl: &ast::Declaration) -> anyhow::Result<()> {
        for specifier in &decl.specifiers {
            self.lower_specifier(specifier)?;
        }
        for decls in &decl.declarators {
            self.lower_declarator(&decls.node.declarator)?;
        }

        Ok(())
    }

    pub fn lower_specifier(
        &mut self,
        specifier: &Node<ast::DeclarationSpecifier>,
    ) -> anyhow::Result<()> {
        if let ast::DeclarationSpecifier::TypeSpecifier(t) = &specifier.node {
            if let ast::TypeSpecifier::Enum(e) = &t.node {
                for en in &e.node.enumerators {
                    self.insert_static(
                        en.node.identifier.node.name.clone(),
                        Static::Variable(None),
                    );
                }
            }
        }
        Ok(())
    }

    pub fn lower_declarator(&mut self, declarator: &Node<ast::Declarator>) -> anyhow::Result<()> {
        if let DeclaratorKind::Identifier(id) = &declarator.node.kind.node {
            let name = id.node.name.clone();
            let mut ret_ownership = None;
            let mut param_ownerships = vec![];
            let mut is_function = false;

            for derived_decl in &declarator.node.derived {
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

            self.insert_static(name, sttc);
        }
        Ok(())
    }
}

pub fn lower_body<'a>(
    name: String,
    fd: &ast::FunctionDefinition,
    statics: &'a CfgStatics,
) -> Result<CfgBody<'a>> {
    let mut ctx = LowerCtx::new(name, statics);
    ctx.add_argument_locals(&fd.declarator.node.derived);
    let start = ctx.new_basic_block();
    let mut active_bb = ActiveBb::new(start);
    ctx.lower_statement(&fd.statement, &mut active_bb)?;
    if ctx.result.basic_blocks[active_bb.idx].terminator.is_none() {
        let mut span = fd.statement.span;
        span.start = span.end - 1;
        ctx.result.basic_blocks[active_bb.idx].terminator = Some(Terminator::Return(span, true))
    }
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
            eprintln!("-- resetting terminator ignored --");
        } else {
            bb.terminator = Some(terminator);
        }
    }

    fn lower_statement(
        &mut self,
        statement: &Node<ast::Statement>,
        active_bb: &mut ActiveBb,
    ) -> Result<()> {
        match &statement.node {
            ast::Statement::Return(e) => {
                if let Some(e) = e {
                    let rvalue = self.expr_to_rvalue(&e, active_bb)?;
                    self.push_assignment(
                        return_slot().into(),
                        rvalue,
                        active_bb,
                        e.span,
                        e.span,
                        e.span,
                    );
                }
                self.set_terminator(Terminator::Return(statement.span, false), active_bb.idx);
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
                            // We can ignore static asserts for now
                        }
                    }
                }
                Ok(())
            }
            ast::Statement::Expression(expr) => {
                let Some(expr) = expr else { return Ok(()) };
                let old_bb = active_bb.idx;
                let rvalue = self.expr_to_rvalue(&expr, active_bb)?;
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
                                let rvalue = self.expr_to_rvalue(&expr, active_bb)?;
                                self.push_discard_if_required(rvalue, active_bb, fr.span);
                            }
                            ast::ForInitializer::Declaration(decl) => {
                                self.lower_declaration(&decl.node, active_bb)?;
                            }
                            ast::ForInitializer::StaticAssert(_) => {
                                // We can ignore static asserts for now
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
                    let rvalue = self.expr_to_rvalue(&step, active_bb)?;
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
                let Some(continue_bb) = active_bb.continue_bb else {
                    return Err(
                        anyhow!("unexpected statement: continue is not used in a loop").into(),
                    );
                };
                self.set_terminator(Terminator::Goto(continue_bb), active_bb.idx);
                Ok(())
            }
            ast::Statement::Break => {
                let Some(break_bb) = active_bb.break_bb else {
                    return Err(anyhow!("unexpected statement: break is not used in a loop").into());
                };
                self.set_terminator(Terminator::Goto(break_bb), active_bb.idx);
                Ok(())
            }
            ast::Statement::If(eef) => self.lower_if_statement(&eef.node, active_bb),
            ast::Statement::Labeled(_)
            | ast::Statement::Switch(_)
            | ast::Statement::DoWhile(_)
            | ast::Statement::Goto(_)
            | ast::Statement::Asm(_) => Err(CfgLowerError::UnsupportedStatement(statement.span)),
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
    ) -> Result<()> {
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
    ) -> Result<()> {
        for declarator in &declaration.declarators {
            let place = match &declarator.node.declarator.node.kind.node {
                DeclaratorKind::Identifier(identifier) => self.named_local(&identifier.node.name),
                DeclaratorKind::Abstract | DeclaratorKind::Declarator(_) => {
                    return Err(anyhow!("not supported: unknown declarator kind").into());
                }
            };

            if let Some(init) = &declarator.node.initializer {
                match &init.node {
                    ast::Initializer::Expression(expr) => {
                        let rvalue = self.expr_to_rvalue(&expr, active_bb)?;
                        self.push_assignment(
                            place.into(),
                            rvalue,
                            active_bb,
                            init.span,
                            declarator.node.declarator.node.kind.span,
                            init.span,
                        );
                        return Ok(());
                    }
                    ast::Initializer::List(_) => {
                        return Err(anyhow!("not supported: unknown declarator initializer").into());
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
        lhs_span: Span,
        rhs_span: Span,
    ) -> Place {
        self.push_statement(
            Statement::Assign(place.clone(), rvalue, span, lhs_span, rhs_span),
            active_bb,
        );
        place
    }

    fn push_temp_assignment(
        &mut self,
        rvalue: Rvalue,
        active_bb: &mut ActiveBb,
        span: Span,
    ) -> Place {
        let place = self.temp_local().into();
        self.push_assignment(place, rvalue, active_bb, span, span, span)
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
                Operand::Place(temp, span)
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
            Rvalue::Use(Operand::Place(place, _)) => place,
            _ => self.push_temp_assignment(rvalue, active_bb, span),
        }
    }

    fn expr_to_rvalue(
        &mut self,
        expr: &Node<ast::Expression>,
        active_bb: &mut ActiveBb,
    ) -> Result<Rvalue> {
        match &expr.node {
            ast::Expression::Identifier(id) => {
                let operand = match self.resolve_identifier(&id.node) {
                    Ok(local) => Operand::Place(local.into(), expr.span),
                    Err(_) => match self.resolve_identifier_static(&id.node) {
                        Ok(sttc) => Operand::Place(sttc.into(), expr.span),
                        Err(_) => {
                            return Err(CfgLowerError::UndefinedIdentifier(id.span));
                        }
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
                        Rvalue::Use(Operand::Place(lhs, expr.span))
                    }
                    ast::BinaryOperator::Assign => {
                        let lhs = self.expr_to_operand_place(&bo.node.lhs, active_bb)?;
                        let rhs = self.expr_to_rvalue(&bo.node.rhs, active_bb)?;
                        let place = self.push_assignment(
                            lhs,
                            rhs,
                            active_bb,
                            bo.span,
                            bo.node.lhs.span,
                            bo.node.rhs.span,
                        );
                        Rvalue::Use(Operand::Place(place, expr.span))
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
                        return Err(CfgLowerError::UnsupportedBinaryOperator(
                            bo.node.operator.span,
                        ));
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
                            _ => {
                                return Err(CfgLowerError::UnsupportedBinaryOperator(
                                    bo.node.operator.span,
                                ));
                            }
                        };
                        Rvalue::BinaryOp(op_kind, lhs, rhs)
                    }
                };

                Ok(rvalue)
            }
            ast::Expression::Constant(constant) => Ok(Rvalue::Use(Operand::Constant(
                ConstOperand::CConst(constant.node.clone()),
                expr.span,
            ))),
            ast::Expression::StringLiteral(literals) => Ok(Rvalue::Use(Operand::Constant(
                ConstOperand::StringLiteral(literals.node.clone()),
                expr.span,
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
                            Operand::Place(operand.clone(), unary.node.operand.span),
                            Self::integer_operand("1", expr.span),
                        ),
                        active_bb,
                        unary.span,
                        unary.node.operand.span,
                        unary.node.operator.span,
                    );
                    Ok(Rvalue::Use(Operand::Place(place, expr.span)))
                }
                ast::UnaryOperator::PostDecrement => {
                    let operand = self.expr_to_operand_place(&unary.node.operand, active_bb)?;
                    let place = self.push_assignment(
                        operand.clone(),
                        Rvalue::BinaryOp(
                            BinaryOpKind::Sub,
                            Operand::Place(operand.clone(), unary.node.operand.span),
                            Self::integer_operand("1", expr.span),
                        ),
                        active_bb,
                        unary.span,
                        unary.node.operand.span,
                        unary.node.operator.span,
                    );
                    Ok(Rvalue::Use(Operand::Place(place, expr.span)))
                }
                ast::UnaryOperator::PreIncrement => {
                    let operand = self.expr_to_operand_place(&unary.node.operand, active_bb)?;
                    let place = self.push_temp_assignment(
                        Rvalue::Use(Operand::Place(operand.clone(), unary.node.operand.span)),
                        active_bb,
                        unary.span,
                    );
                    self.push_assignment(
                        operand.clone(),
                        Rvalue::BinaryOp(
                            BinaryOpKind::Add,
                            Operand::Place(operand.clone(), unary.node.operand.span),
                            Self::integer_operand("1", expr.span),
                        ),
                        active_bb,
                        unary.span,
                        unary.node.operand.span,
                        unary.node.operator.span,
                    );
                    Ok(Rvalue::Use(Operand::Place(place, expr.span)))
                }
                ast::UnaryOperator::PreDecrement => {
                    let operand = self.expr_to_operand_place(&unary.node.operand, active_bb)?;
                    let place = self.push_temp_assignment(
                        Rvalue::Use(Operand::Place(operand.clone(), unary.node.operand.span)),
                        active_bb,
                        unary.span,
                    );
                    self.push_assignment(
                        operand.clone(),
                        Rvalue::BinaryOp(
                            BinaryOpKind::Sub,
                            Operand::Place(operand.clone(), expr.span),
                            Self::integer_operand("1", expr.span),
                        ),
                        active_bb,
                        unary.span,
                        unary.node.operand.span,
                        unary.node.operator.span,
                    );
                    Ok(Rvalue::Use(Operand::Place(place, expr.span)))
                }
                ast::UnaryOperator::Indirection
                | ast::UnaryOperator::Plus
                | ast::UnaryOperator::Minus
                | ast::UnaryOperator::Complement
                | ast::UnaryOperator::Negate => Err(CfgLowerError::UnsupportedUnaryOperator(
                    unary.node.operator.span,
                )),
            },
            ast::Expression::Call(call) => {
                let callee = self.expr_to_operand(&call.node.callee, active_bb)?;
                let args = call
                    .node
                    .arguments
                    .iter()
                    .map(|x| self.expr_to_operand(&x, active_bb))
                    .collect::<Result<Vec<Operand>>>()?;
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
                Ok(Rvalue::Use(Operand::Place(destination.into(), call.span)))
            }
            ast::Expression::Conditional(c) => {
                let destination: Place = self.temp_local().into();
                let dest_bb = self.new_basic_block();
                let condition = self.expr_to_operand(&c.node.condition, active_bb)?;
                let then_bb = self.new_basic_block();
                let else_bb = self.new_basic_block();
                self.set_terminator(
                    Terminator::SwitchInt(condition, vec![1], vec![then_bb, else_bb]),
                    active_bb.idx,
                );
                active_bb.switch(then_bb);
                let then_rvalue = self.expr_to_rvalue(&c.node.then_expression, active_bb)?;
                self.push_assignment(
                    destination.clone(),
                    then_rvalue,
                    active_bb,
                    c.span,
                    c.node.then_expression.span,
                    c.span,
                );
                self.set_terminator(Terminator::Goto(dest_bb), active_bb.idx);
                active_bb.switch(else_bb);
                let else_rvalue = self.expr_to_rvalue(&c.node.else_expression, active_bb)?;
                self.push_assignment(
                    destination.clone(),
                    else_rvalue,
                    active_bb,
                    c.span,
                    c.node.else_expression.span,
                    c.span,
                );
                self.set_terminator(Terminator::Goto(dest_bb), active_bb.idx);
                active_bb.switch(dest_bb);
                Ok(Rvalue::Use(Operand::Place(destination, c.span)))
            }
            ast::Expression::Comma(c) => {
                let (last, intermediates) = c.split_last().expect("Invalid comma expression");
                for exp in intermediates {
                    _ = self.expr_to_rvalue(exp, active_bb)?;
                }
                self.expr_to_rvalue(last, active_bb)
            }
            // We don't consider types yet, so cast is a no op
            ast::Expression::Cast(c) => self.expr_to_rvalue(&c.node.expression, active_bb),
            ast::Expression::GenericSelection(_)
            | ast::Expression::Member(_)
            | ast::Expression::CompoundLiteral(_)
            | ast::Expression::SizeOfTy(_)
            | ast::Expression::SizeOfVal(_)
            | ast::Expression::AlignOf(_)
            | ast::Expression::OffsetOf(_)
            | ast::Expression::VaArg(_)
            | ast::Expression::Statement(_) => {
                return Err(CfgLowerError::UnsupportedExpression(expr.span));
            }
        }
    }

    fn expr_to_operand(
        &mut self,
        expr: &Node<ast::Expression>,
        active_bb: &mut ActiveBb,
    ) -> Result<Operand> {
        let rvalue = self.expr_to_rvalue(&expr, active_bb)?;
        Ok(self.rvalue_to_operand(
            rvalue,
            active_bb,
            Span {
                start: expr.span.start + 1,
                end: expr.span.end,
            },
        ))
    }

    fn expr_to_operand_place(
        &mut self,
        expr: &Node<ast::Expression>,
        active_bb: &mut ActiveBb,
    ) -> Result<Place> {
        let rvalue = self.expr_to_rvalue(&expr, active_bb)?;
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

    pub fn integer_operand(number: &str, span: Span) -> Operand {
        Operand::Constant(
            ConstOperand::CConst(ast::Constant::Integer(ast::Integer {
                base: ast::IntegerBase::Decimal,
                number: number.into(),
                suffix: IntegerSuffix {
                    size: ast::IntegerSize::Int,
                    unsigned: false,
                    imaginary: false,
                },
            })),
            span,
        )
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
