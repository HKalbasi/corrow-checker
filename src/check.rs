use std::collections::HashMap;

use crepe::crepe;
use la_arena::ArenaMap;

use crate::cfg::{CfgBody, Operand, Place, RawPlace, Rvalue, Statement, Terminator};
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
    struct Assign(PlaceId, PlaceId, NodeId);

    struct PlaceOwnValue(PlaceId, NodeId, Span);

    @output
    #[derive(Debug)]
    pub struct LeakByAssign(pub Span, pub Span);

    @output
    #[derive(Debug)]
    pub struct LeakByReturn(pub Span, pub Span);

    PlaceOwnValue(p, n, span) <- PlaceFilledByOwnedValue(p, n, span);
    PlaceOwnValue(p, n, span) <- PlaceOwnValue(p2, n, span), Assign(p, p2, n);
    PlaceOwnValue(p, n, span) <- PlaceOwnValue(p, n1, span), ControlFlowGoes(n1, n), !PlacePrevValueLostByAssign(p, n, _), !Assign(_, p, n1);
    LeakByAssign(own_span, lost_span) <- PlaceOwnValue(p, n1, own_span), ControlFlowGoes(n1, n), PlacePrevValueLostByAssign(p, n, lost_span);
    LeakByReturn(own_span, return_span) <- PlaceOwnValue(_, n, own_span), ControlFlowReturns(n, return_span);
}

#[derive(Debug)]
pub enum CheckError {
    LeakByAssign(LeakByAssign),
    LeakByReturn(LeakByReturn),
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
                    Statement::Assign(p, r, span) => {
                        let p = self.place_id_of(p);
                        self.crepe
                            .placeprevvaluelostbyassign
                            .push(PlacePrevValueLostByAssign(p, nn, *span));
                        self.add_control_flow_link(cur, nn);
                        cur = nn;
                        nn = self.add_node();
                        match r {
                            Rvalue::Use(p2) => match p2 {
                                Operand::Place(p2) => {
                                    let p2 = self.place_id_of(p2);
                                    self.crepe.assign.push(Assign(p, p2, nn));
                                }
                                Operand::Constant(_) => (),
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
            let terminator = bb_data.terminator.as_ref().unwrap();
            match terminator {
                Terminator::Return(span) => {
                    self.add_control_flow_link(before_terminator[bb], end_of_block[bb]);
                    self.crepe
                        .controlflowreturns
                        .push(ControlFlowReturns(end_of_block[bb], *span));
                }
                Terminator::Call {
                    callee,
                    args: _,
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
                    if let Operand::Place(place) = callee {
                        if let RawPlace::Static(idx) = &place.raw {
                            if cfg.statics.name_to_static.contains_key("malloc") {
                                self.crepe
                                    .placefilledbyownedvalue
                                    .push(PlaceFilledByOwnedValue(p, n2, *span));
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
                Terminator::SwitchInt(_, _, targets) => {
                    self.add_control_flow_link(before_terminator[bb], end_of_block[bb]);
                    for target in targets {
                        self.add_control_flow_link(end_of_block[bb], start_of_block[*target]);
                    }
                }
            }
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
    let (leak_by_assign, leak_by_return) = datalog.run();
    leak_by_assign
        .into_iter()
        .map(CheckError::LeakByAssign)
        .chain(leak_by_return.into_iter().map(CheckError::LeakByReturn))
        .collect()
}
