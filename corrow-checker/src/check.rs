use std::collections::HashMap;

use crepe::crepe;
use la_arena::ArenaMap;

use crate::{
    cfg::{CfgBody, Operand, Ownership, Place, RawPlace, Rvalue, Statement, Static, Terminator},
    report::OwnershipReason,
};
use lang_c::span::Span;

crepe! {
    @input
    #[derive(Debug)]
    struct PlaceId(u32);

    @input
    #[derive(Debug)]
    struct NodeId(u32);

    @input
    #[derive(Debug)]
    struct ControlFlowGoes(NodeId, NodeId);

    @input
    #[derive(Debug)]
    struct ControlFlowReturns(NodeId, Span);

    @input
    #[derive(Debug)]
    struct PlaceFilledByOwnedValue(PlaceId, NodeId, Span);

    @input
    #[derive(Debug)]
    struct PlacePrevValueLostByAssign(PlaceId, NodeId, Span);

    @input
    #[derive(Debug)]
    struct OwnershipLeft(PlaceId, NodeId, Span);

    @input
    #[derive(Debug)]
    struct Assign(PlaceId, PlaceId, NodeId, Span);

    @input
    #[derive(Debug)]
    struct PlaceUsed(PlaceId, NodeId, Span);

    struct PlaceMayDangle(PlaceId, NodeId, Span);

    struct PlaceOwnValue(PlaceId, NodeId, OwnershipReason);

    @output
    #[derive(Debug)]
    pub struct LeakByAssign(pub OwnershipReason, pub Span);

    @output
    #[derive(Debug)]
    pub struct LeakByReturn(pub OwnershipReason, pub Span);

    @output
    #[derive(Debug)]
    pub struct UseAfterMove(pub Span, pub Span);

    PlaceOwnValue(p, n, OwnershipReason::new(intro_span)) <- PlaceFilledByOwnedValue(p, n, intro_span);
    PlaceOwnValue(p, n, reason.with_move(move_span)) <- PlaceOwnValue(p2, n, reason), Assign(p, p2, n, move_span);
    PlaceOwnValue(p, n, reason) <- PlaceOwnValue(p, n1, reason), ControlFlowGoes(n1, n), !PlacePrevValueLostByAssign(p, n, _), !Assign(_, p, n1, _), !OwnershipLeft(p, n1, _);

    PlaceMayDangle(p, n, lost_span) <- PlaceOwnValue(p, n, _), Assign(_, p, n, lost_span);
    PlaceMayDangle(p, n, lost_span) <- PlaceOwnValue(p, n, _), OwnershipLeft(p, n, lost_span);
    PlaceMayDangle(p, n, lost_span) <- PlaceMayDangle(p, n1, lost_span), ControlFlowGoes(n1, n), !Assign(p, _, n, _);

    LeakByAssign(own_reason, lost_span) <- PlaceOwnValue(p, n1, own_reason), ControlFlowGoes(n1, n), PlacePrevValueLostByAssign(p, n, lost_span);
    LeakByReturn(own_reason, return_span) <- PlaceOwnValue(_, n, own_reason), ControlFlowReturns(n, return_span);
    UseAfterMove(move_span, use_span) <- PlaceMayDangle(p, n1, move_span), ControlFlowGoes(n1, n), PlaceUsed(p, n, use_span);
}

#[derive(Debug, PartialEq, Eq)]
pub enum CheckError {
    LeakByAssign(LeakByAssign),
    LeakByReturn(LeakByReturn),
    UseAfterMove(UseAfterMove),
}

impl CheckError {
    pub fn compare_for_stability(&self) -> impl Ord {
        match self {
            CheckError::LeakByAssign(e) => (1, e.1.start, e.1.end),
            CheckError::LeakByReturn(e) => (1, e.1.start, e.1.end),
            CheckError::UseAfterMove(e) => (1, e.1.start, e.1.end),
        }
    }
}

#[derive(Default)]
struct CrepeFiller {
    crepe: Crepe,
    place_id_map: HashMap<Place, PlaceId>,
}

impl CrepeFiller {
    fn add_node(&mut self) -> NodeId {
        let id = self.crepe.nodeid.len() as u32;
        self.crepe.nodeid.push(NodeId(id));
        NodeId(id)
    }

    fn add_place(&mut self) -> PlaceId {
        let id = self.crepe.placeid.len() as u32;
        self.crepe.placeid.push(PlaceId(id));
        PlaceId(id)
    }

    fn build(cfg: &CfgBody) -> Crepe {
        let mut this = Self::default();
        this.fill_cfg(cfg);
        this.crepe
    }

    fn add_control_flow_link(&mut self, from: NodeId, to: NodeId) {
        self.crepe.controlflowgoes.push(ControlFlowGoes(from, to));
    }

    fn fill_cfg(&mut self, cfg: &CfgBody) {
        let mut start_of_block = ArenaMap::new();
        let mut before_terminator = ArenaMap::new();
        let mut end_of_block = ArenaMap::new();
        for (bb, _) in cfg.basic_blocks.iter() {
            start_of_block.insert(bb, self.add_node());
            before_terminator.insert(bb, self.add_node());
            end_of_block.insert(bb, self.add_node());
        }
        for (bb, bb_data) in cfg.basic_blocks.iter() {
            let mut cur = start_of_block[bb];
            for stmt in &bb_data.statements {
                let mut nn = self.add_node();
                match stmt {
                    Statement::Assign(place, r, span, lhs_span, rhs_span) => {
                        let p = self.place_id_of(place);
                        self.crepe
                            .placeprevvaluelostbyassign
                            .push(PlacePrevValueLostByAssign(p, nn, *span));
                        if place.projections.len() > 0 {
                            let p = self.place_id_of(&place.without_projections());
                            self.crepe.placeused.push(PlaceUsed(p, nn, *lhs_span));
                        }
                        self.add_control_flow_link(cur, nn);
                        cur = nn;
                        nn = self.add_node();
                        // TODO: handle ptr = NULL
                        match r {
                            Rvalue::Use(p2) => match p2 {
                                Operand::Place(p2, _) => {
                                    let p2 = self.place_id_of(p2);
                                    self.crepe.assign.push(Assign(p, p2, nn, *span));
                                    self.crepe.placeused.push(PlaceUsed(p2, nn, *rhs_span));
                                }
                                Operand::Constant(..) => (),
                            },
                            Rvalue::Ref(_) => (),
                            Rvalue::BinaryOp(_, _, _) => (),
                        }
                    }
                }
                self.add_control_flow_link(cur, nn);
                cur = nn;
            }
            self.add_control_flow_link(cur, before_terminator[bb]);
        }
        for (bb, bb_data) in cfg.basic_blocks.iter() {
            let terminator = bb_data
                .terminator
                .as_ref()
                .expect("Terminator should be None only in construction");
            match terminator {
                Terminator::Return(span) => {
                    self.add_control_flow_link(before_terminator[bb], end_of_block[bb]);
                    self.crepe
                        .controlflowreturns
                        .push(ControlFlowReturns(end_of_block[bb], *span));
                }
                Terminator::Call {
                    callee,
                    args,
                    return_place: p,
                    target,
                    span,
                } => {
                    let n1 = self.add_node();
                    let n2 = self.add_node();
                    let p = self.place_id_of(p);
                    self.crepe
                        .placeprevvaluelostbyassign
                        .push(PlacePrevValueLostByAssign(p, n1, *span));

                    self.add_place_use_if_applicable(callee, n1);

                    for arg in args {
                        self.add_place_use_if_applicable(arg, n1);
                    }

                    if let Operand::Place(place, _) = callee {
                        if let RawPlace::Static(idx) = &place.raw {
                            let sttc = &cfg.statics.statics[idx.clone()];
                            if let Static::Function(ret, params) = sttc {
                                if let Some(Ownership::Owned) = ret {
                                    self.crepe
                                        .placefilledbyownedvalue
                                        .push(PlaceFilledByOwnedValue(p, n2, *span));
                                }

                                for (i, arg) in args.iter().enumerate() {
                                    if let Operand::Place(arg_place, arg_span) = arg {
                                        if let Some(Some(Ownership::Owned)) = params.get(i) {
                                            let arg_place_id = self.place_id_of(arg_place);
                                            self.crepe.ownershipleft.push(OwnershipLeft(
                                                arg_place_id,
                                                n1,
                                                *arg_span,
                                            ))
                                        }
                                    }
                                }
                            }
                        }
                    }
                    self.add_control_flow_link(before_terminator[bb], n1);
                    self.add_control_flow_link(n1, n2);
                    self.add_control_flow_link(n2, end_of_block[bb]);
                    self.add_control_flow_link(end_of_block[bb], start_of_block[*target]);
                }
                Terminator::Goto(target) => {
                    self.add_control_flow_link(before_terminator[bb], end_of_block[bb]);
                    self.add_control_flow_link(end_of_block[bb], start_of_block[*target]);
                }
                Terminator::SwitchInt(operand, _, targets) => {
                    self.add_place_use_if_applicable(operand, before_terminator[bb]);
                    self.add_control_flow_link(before_terminator[bb], end_of_block[bb]);
                    for target in targets {
                        self.add_control_flow_link(end_of_block[bb], start_of_block[*target]);
                    }
                }
            }
        }
    }

    fn add_place_use_if_applicable(&mut self, operand: &Operand, node_id: NodeId) {
        if let Operand::Place(place, span) = operand {
            let p = self.place_id_of(place);
            self.crepe.placeused.push(PlaceUsed(p, node_id, *span));
        }
    }

    fn place_id_of(&mut self, p: &Place) -> PlaceId {
        match self.place_id_map.get(p) {
            Some(id) => *id,
            None => {
                let v = self.add_place();
                self.place_id_map.insert(p.clone(), v);
                v
            }
        }
    }
}

pub fn check_cfg(cfg: &CfgBody) -> Vec<CheckError> {
    let datalog = CrepeFiller::build(cfg);
    let (leak_by_assign, leak_by_return, use_after_move) = datalog.run();
    leak_by_assign
        .into_iter()
        .map(CheckError::LeakByAssign)
        .chain(leak_by_return.into_iter().map(CheckError::LeakByReturn))
        .chain(use_after_move.into_iter().map(CheckError::UseAfterMove))
        .collect()
}
