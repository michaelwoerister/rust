// Copyright 2017 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use hir::def_id::{DefId, DefIndex, CRATE_DEF_INDEX};
use hir;
use syntax::ast::NodeId;
use std::collections::BTreeMap;
use hir::itemlikevisit::ItemLikeVisitor;

pub fn check_crate<'hir>(hir_map: &hir::map::Map<'hir>) {
    let mut outer_visitor = OuterVisitor {
        hir_map: hir_map
    };

    hir_map.dep_graph.with_ignore(|| {
        hir_map.krate().visit_all_item_likes(&mut outer_visitor);
    });
}

struct StableNodeIdSanityCheck<'a, 'hir: 'a> {
    hir_map: &'a hir::map::Map<'hir>,
    owner_def_index: Option<DefIndex>,
    stable_ids_seen: BTreeMap<hir::StableNodeId, NodeId>,
}

struct OuterVisitor<'a, 'hir: 'a> {
    hir_map: &'a hir::map::Map<'hir>,
}

impl<'a, 'hir: 'a> OuterVisitor<'a, 'hir> {
    fn new_inner_visitor(&self,
                         hir_map: &'a hir::map::Map<'hir>)
                         -> StableNodeIdSanityCheck<'a, 'hir> {
        StableNodeIdSanityCheck {
            hir_map: hir_map,
            owner_def_index: None,
            stable_ids_seen: BTreeMap::new(),
        }
    }
}

impl<'a, 'hir: 'a> ItemLikeVisitor<'hir> for OuterVisitor<'a, 'hir> {
    fn visit_item(&mut self, i: &'hir hir::Item) {
        let mut inner_visitor = self.new_inner_visitor(self.hir_map);
        inner_visitor.check(i.id, |this| hir::intravisit::walk_item(this, i));
    }

    fn visit_trait_item(&mut self, i: &'hir hir::TraitItem) {
        let mut inner_visitor = self.new_inner_visitor(self.hir_map);
        inner_visitor.check(i.id, |this| hir::intravisit::walk_trait_item(this, i));
    }

    fn visit_impl_item(&mut self, i: &'hir hir::ImplItem) {
        let mut inner_visitor = self.new_inner_visitor(self.hir_map);
        inner_visitor.check(i.id, |this| hir::intravisit::walk_impl_item(this, i));
    }
}

impl<'a, 'hir: 'a> StableNodeIdSanityCheck<'a, 'hir> {

    fn check<F: FnOnce(&mut StableNodeIdSanityCheck<'a, 'hir>)>(&mut self,
                                                                node_id: NodeId,
                                                                walk: F) {
        assert!(self.owner_def_index.is_none());
        let owner_def_index = self.hir_map.local_def_id(node_id).index;
        self.owner_def_index = Some(owner_def_index);
        walk(self);

        if owner_def_index == CRATE_DEF_INDEX {
            return
        }

        // There's always at least one entry for the owning item itself
        let max = self.stable_ids_seen
                      .keys()
                      .map(|stable_id| stable_id.local_id)
                      .max()
                      .unwrap();

        if max as usize != self.stable_ids_seen.len() - 1 {
            use std::iter::FromIterator;
            bug!("Local node IDs not assigned densely in {}: {:?}",
                 self.hir_map.def_path(DefId::local(owner_def_index)).to_string_no_crate(),
                 Vec::from_iter(self.stable_ids_seen.keys()));
        }
    }
}

impl<'a, 'hir: 'a> hir::intravisit::Visitor<'hir> for StableNodeIdSanityCheck<'a, 'hir> {

    fn nested_visit_map<'this>(&'this mut self) -> hir::intravisit::NestedVisitorMap<'this, 'hir> {
        hir::intravisit::NestedVisitorMap::OnlyBodies(self.hir_map)
    }

    fn visit_id(&mut self, node_id: NodeId) {
        let owner = self.owner_def_index.unwrap();
        let stable_id = self.hir_map.definitions().ast_to_hir_node_id[node_id.as_usize()];

        if stable_id == hir::DUMMY_NODE_ID {
            bug!("No StableNodeId assigned for NodeId {}: {:?}",
                 node_id,
                 self.hir_map.node_to_string(node_id));
        }

        if owner != stable_id.owner {
            bug!("StableNodeIdSanityCheck: The recorded owner of {} is {} instead of {}",
                 self.hir_map.node_to_string(node_id),
                 self.hir_map.def_path(DefId::local(stable_id.owner)).to_string_no_crate(),
                 self.hir_map.def_path(DefId::local(owner)).to_string_no_crate());
        }

        if let Some(prev) = self.stable_ids_seen.insert(stable_id, node_id) {
            if prev != node_id {
                bug!("Same StableNodeId {}/{} assigned for nodes {} and {}",
                     self.hir_map.def_path(DefId::local(stable_id.owner)).to_string_no_crate(),
                     stable_id.local_id,
                     self.hir_map.node_to_string(prev),
                     self.hir_map.node_to_string(node_id));
            }
        }
    }

    fn visit_impl_item_ref(&mut self, _: &'hir hir::ImplItemRef) {
        // Explicitly do nothing here.
    }
}
