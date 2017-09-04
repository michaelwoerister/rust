// Copyright 2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use ich::Fingerprint;
use rustc_data_structures::fx::FxHashMap;
use std::thread;
use super::dep_node::DepNode;
use super::serialized::{SerializedDepGraph, SerializedDepNodeIndex};

#[derive(Debug, RustcEncodable, RustcDecodable)]
pub struct PreviousDepGraph {
    data: SerializedDepGraph,
    index: FxHashMap<DepNode, SerializedDepNodeIndex>
}

impl PreviousDepGraph {

    pub fn start_loading() -> thread::JoinHandle<PreviousDepGraph> {
        panic!()
    }

    fn new(data: SerializedDepGraph) -> PreviousDepGraph {
        let index: FxHashMap<_, _> = data.nodes
                                         .iter_enumerated()
                                         .map(|(idx, &dep_node)| (dep_node, idx))
                                         .collect();
        PreviousDepGraph {
            data,
            index,
        }
    }

    // FIXME: Use return an Iterator here
    pub fn edges_from(&self, dep_node: DepNode) -> Vec<DepNode> {
        let node_index = self.index[&dep_node];
        self.data.edge_targets_from(node_index).iter().map(|&node_index| {
            self.data.nodes[node_index]
        }).collect()
    }

    pub fn fingerprint_of(_dep_node: DepNode) -> Fingerprint {
        panic!("not yet implemented")
    }
}
