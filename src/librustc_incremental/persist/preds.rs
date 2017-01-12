// Copyright 2012-2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use rustc::dep_graph::{DepGraphQuery, DepNode};
use rustc::hir::def_id::DefId;
use rustc_data_structures::fx::{FxHashMap, FxHashSet};
use rustc_data_structures::graph::{
    // DepthFirstTraversal, INCOMING,
     NodeIndex, Graph};

use super::hash::*;
use ich::Fingerprint;

/// A data-structure that makes it easy to enumerate the hashable
/// predecessors of any given dep-node.
pub struct Predecessors<'query> {
    // - Keys: dep-nodes that may have work-products, output meta-data
    //   nodes.
    // - Values: transitive predecessors of the key that are hashable
    //   (e.g., HIR nodes, input meta-data nodes)
    pub inputs: FxHashMap<&'query DepNode<DefId>, Vec<&'query DepNode<DefId>>>,

    // - Keys: some hashable node
    // - Values: the hash thereof
    pub hashes: FxHashMap<&'query DepNode<DefId>, Fingerprint>,
}


// fn find_roots<'a>(tcx: ::rustc::ty::TyCtxt,
//                   graph: &Graph<DepNode<DefId>, ()>,
//                   visit_counts: &mut [u32],
//                   node: u32,
//                   cache: &mut [Option<Box<[u32]>>],
//                   visited: &mut FxHashSet<u32>,
//                   roots: &mut FxHashSet<u32>,
//                   write_to_cache: bool,
//                   caching_threshold: u32)
// {
//     if !visited.insert(node) {
//         return;
//     }

//     if HashContext::is_hashable(graph.node_data(NodeIndex(node as usize))) {
//         roots.insert(node);
//     } else {
//         if let Some(ref cached) = cache[node as usize] {
//             // ::rustc::util::common::record_time(&tcx.sess.perf_stats.read_dep_graph_cache, || {
//                 roots.extend(cached.iter());
//             // });
//             return
//         }

//         visit_counts[node as usize] += 1;

//         if write_to_cache {
//             if visit_counts[node as usize] > caching_threshold {
//                 // ::rustc::util::common::record_time(&tcx.sess.perf_stats.build_dep_graph_caches, move || {
//                     let mut sub_roots = FxHashSet();
//                     let mut sub_visited = FxHashSet();
//                     find_roots(tcx, graph, visit_counts, node, cache, &mut sub_visited, &mut sub_roots, false, caching_threshold);

//                     // let cached_roots: Vec<NodeIndex> = DepthFirstTraversal::with_start_node(&graph, node, INCOMING)
//                     //     .filter(|&n| HashContext::is_hashable(graph.node_data(n)))
//                     //     .collect();

//                     roots.extend(sub_roots.iter());
//                     cache[node as usize] = Some(sub_roots.into_iter().collect::<Vec<_>>().into_boxed_slice());
//                 // });
//                 // roots.extend(cached_roots.iter().cloned());
//                 // cache[index] = Some(cached_roots.into_boxed_slice());
//                 return;
//             }
//         }

//         for pred in graph.predecessor_nodes(NodeIndex(node as usize)) {
//             find_roots(tcx, graph, visit_counts, pred.node_id() as u32, cache, visited, roots, write_to_cache, caching_threshold);
//         }
//     }
// }

fn dedup(data: &mut Vec<u32>, set: &mut FxHashSet<u32>) {
    if data.len() <= 64 {
        data.sort();
        data.dedup();
    } else {
        set.clear();

        let mut i = 0;
        let mut c = data.len();

        while i < c {
            let x = data[i];
            if set.insert(x) {
                i += 1;
            } else {
                c -= 1;
                data[i] = data[c];
            }
        }

        data.truncate(c);
    }
}

// fn find_roots<'a>(_tcx: ::rustc::ty::TyCtxt,
//                   graph: &Graph<DepNode<DefId>, ()>,
//                   visit_counts: &mut [u32],
//                   node: u32,
//                   cache: &mut [Option<Box<[u32]>>],
//                   visited: &mut FxHashSet<u32>,
//                   roots: &mut FxHashSet<u32>,
//                   write_to_cache: bool,
//                   caching_threshold: u32)
// {
//     let mut stack = vec![node];
//     let mut sub_visited = FxHashSet();
//     let mut sub_roots = FxHashSet();

//     while stack.len() > 0 {
//         let node = stack.pop().unwrap();

//         if !visited.insert(node) {
//             continue
//         }

//         if HashContext::is_hashable(graph.node_data(NodeIndex(node as usize))) {
//             roots.insert(node);
//         } else {
//             if let Some(ref cached) = cache[node as usize] {
//                 roots.extend(cached.iter());
//                 continue
//             }

//             visit_counts[node as usize] += 1;

//             if write_to_cache {
//                 if visit_counts[node as usize] > caching_threshold {
//                     sub_roots.clear();
//                     sub_visited.clear();
//                     find_roots(_tcx, graph, visit_counts, node, cache, &mut sub_visited, &mut sub_roots, false, caching_threshold);
//                     let sub_roots: Vec<u32> = sub_roots.iter().cloned().collect();
//                     roots.extend(sub_roots.iter());
//                     cache[node as usize] = Some(sub_roots.into_boxed_slice());
//                     continue
//                 }
//             }

//             for pred in graph.predecessor_nodes(NodeIndex(node as usize)) {
//                 stack.push(pred.node_id() as u32);
//             }
//         }
//     }
// }


fn find_roots<'a>(_tcx: ::rustc::ty::TyCtxt,
                  graph: &Graph<DepNode<DefId>, ()>,
                  visit_counts: &mut [u32],
                  node: u32,
                  cache: &mut [Option<Box<[u32]>>],
                  visited: &mut FxHashSet<u32>,
                  roots: &mut Vec<u32>,
                  write_to_cache: bool,
                  caching_threshold: u32)
{
    let mut stack = vec![node];
    let mut sub_visited = FxHashSet();
    let mut sub_roots = Vec::new();

    while stack.len() > 0 {
        let node = stack.pop().unwrap();

        if !visited.insert(node) {
            continue
        }

        if HashContext::is_hashable(graph.node_data(NodeIndex(node as usize))) {
            roots.push(node);
        } else {
            if let Some(ref cached) = cache[node as usize] {
                roots.extend_from_slice(cached);
                continue
            }

            visit_counts[node as usize] += 1;

            if write_to_cache {
                if visit_counts[node as usize] > caching_threshold {
                    sub_roots.clear();
                    sub_visited.clear();
                    find_roots(_tcx, graph, visit_counts, node, cache, &mut sub_visited, &mut sub_roots, false, caching_threshold);
                    // let sub_roots: Vec<u32> = sub_roots.iter().cloned().collect();
                    dedup(&mut sub_roots, &mut sub_visited);

                    roots.extend_from_slice(&sub_roots[..]);
                    cache[node as usize] = Some(sub_roots.clone().into_boxed_slice());
                    continue
                }
            }

            for pred in graph.predecessor_nodes(NodeIndex(node as usize)) {
                stack.push(pred.node_id() as u32);
            }
        }
    }
}


impl<'q> Predecessors<'q> {
    pub fn new(query: &'q DepGraphQuery<DefId>, hcx: &mut HashContext) -> Self {
        // Find nodes for which we want to know the full set of preds
        let all_nodes = query.graph.all_nodes();
        let tcx = hcx.tcx;
        // let mut dfs = DepthFirstTraversal::new(&query.graph, INCOMING);

        let node_count = query.graph.len_nodes();
        // let mut visit_counts: Vec<u32> = Vec::with_capacity(node_count);
        // visit_counts.resize(node_count, 0);


        let mut vc: Vec<u32> = Vec::new();
        let mut node_cache: Vec<Option<Box<[u32]>>> = Vec::new();

        vc.resize(node_count, 0);
        node_cache.resize(node_count, None);

        let mut visited: FxHashSet<u32> = FxHashSet();
        // let mut roots: FxHashSet<u32> = FxHashSet();
        let mut roots: Vec<u32> = Vec::new();

        let caching_threshold: u32 = ::std::env::var("CACHING_THRESHOLD")
            .unwrap_or("10".to_owned()).parse().unwrap();

        // {
        //     use std::fmt::Write;
        //     // let mut root_counts = Vec::new();

        //     for (node_index, node) in all_nodes.iter().enumerate() {
        //         let mut name = String::new();
        //         write!(name, "{:?}", node.data).unwrap();
        //         if !(name.contains("Tables(DefId") &&
        //              name.contains("::util[0]::small_vector[0]::{{impl}}[3]::zero[0]")) {
        //             continue;
        //         }

        //         // roots.clear();
        //         // visited.clear();

        //         // find_roots(tcx,
        //         //            &query.graph,
        //         //            &mut vc,
        //         //            node_index as u32,
        //         //            &mut node_cache[..],
        //         //            &mut visited,
        //                    // &mut roots,
        //         //            true,
        //         //            caching_threshold);
        //         // println!("preds: {}", roots.len());

        //         let tr = DepthFirstTraversal::with_start_node(&query.graph, NodeIndex(node_index), INCOMING);

        //         let mut counter = 0;
        //         for node in tr {
        //             if HashContext::is_hashable(query.graph.node_data(node)) {
        //                 counter += 1;
        //             }
        //         }

        //         println!("preds: {}", counter);


        //         //if name.contains("CollectItem(DefId { krate: CrateNum(0), node: DefIndex(12133) => syntex_syntax/b5fd21cdd1abe01581b0c0cf4442b59a::ext[0]::source_util[0]::expand_include_str[0] })") {
        //         // if name.contains("CollectItem(DefId { krate: CrateNum(0), node: DefIndex(2455) => syntex_syntax/b5fd21cdd1abe01581b0c0cf4442b59a::feature_gate[0]::{{impl}}[4]::visit_vis[0] })") {
        //         //     for &pred in &roots {
        //         //         println!("{:?}", query.graph.node_data(NodeIndex(pred as usize)));
        //         //     }
        //         // }
        //         // if name.contains("TypeckItemBody(DefId { krate: CrateNum(0), node: DefIndex(533) => syntex_syntax/b5fd21cdd1abe01581b0c0cf4442b59a::util[0]::small_vector[0]::{{impl}}[3] })") {
        //         // }

        //         // root_counts.push((roots.len(), &node.data));
        //     }

        //     // root_counts.sort_by_key(|&(c, _)| c);

        //     // for &(c, node) in &root_counts {

        //     // }

        //     // for (c, node) in root_counts {
        //     //     println!("{:?}: {}", node, c);
        //     // }
        // }

        // ::rustc::util::common::record_time(&tcx.sess.perf_stats.work_products_only, || {

        //     for (node_index, node) in all_nodes.iter().enumerate() {
        //         match node.data {
        //             DepNode::WorkProduct(_) => {}
        //             _ => continue,
        //         };

        //         roots.clear();
        //         visited.clear();

        //         find_roots(tcx,
        //                    &query.graph,
        //                    &mut vc,
        //                    node_index as u32,
        //                    &mut node_cache[..],
        //                    &mut visited,
        //                    &mut roots,
        //                    true,
        //                    caching_threshold);
        //     }
        // });

        let inputs: FxHashMap<_, _> = all_nodes.iter()
            .enumerate()
            .filter(|&(_, node)| match node.data {
                DepNode::WorkProduct(_) => true,
                DepNode::MetaData(ref def_id) => def_id.is_local(),

                // if -Z query-dep-graph is passed, save more extended data
                // to enable better unit testing
                DepNode::TypeckItemBody(_) |
                DepNode::TransCrateItem(_) => tcx.sess.opts.debugging_opts.query_dep_graph,

                _ => false,
            })
            .map(|(node_index, node)| {
                // dfs.reset(NodeIndex(node_index));
                // let inputs: Vec<_> = dfs.by_ref()
                //     .map(|i| &all_nodes[i.node_id()].data)
                //     .filter(|d| HashContext::is_hashable(d))
                //     .collect();
                // let mut inputs: Vec<&DepNode<_>> = Vec::new();
                // for node_index in dfs.by_ref() {
                //     let index = node_index.node_id();

                //     if HashContext::is_hashable(&all_nodes[index].data) {
                //         inputs.push(&all_nodes[index].data)
                //     } else {
                //         visit_counts[index] += 1;

                //         if visit_counts[index] > 100 {

                //         }
                //     }
                // }
                roots.clear();
                visited.clear();
                find_roots(tcx,
                           &query.graph,
                           &mut vc,
                           node_index as u32,
                           &mut node_cache[..],
                           &mut visited,
                           &mut roots,
                           true,
                           caching_threshold);

                dedup(&mut roots, &mut visited);

                // dfs.reset(NodeIndex(node_index));
                // let mut inputs2: Vec<_> = dfs.by_ref()
                //     .map(|i| (i, &all_nodes[i.node_id()].data))
                //     .filter(|&(_, d)| HashContext::is_hashable(d))
                //     .map(|(i, _)| i.node_id() as u32)
                //     .collect();
                // inputs2.sort();
                // roots.sort();

                // assert_eq!(inputs2, roots);

                let inputs: Vec<&DepNode<DefId>> =
                ::rustc::util::common::record_time(&tcx.sess.perf_stats.map_dep_graph_nodes, || {
                     roots.iter().map(|&i| query.graph.node_data(NodeIndex(i as usize))).collect()
                 });




                // if inputs.len() > 10000 {
                    // println!("inputs: {:?} = {}", node.data, inputs.len());
                // }

                (&node.data, inputs)
                // (&node.data, inputs)
            })
            .collect();

        // for (index, count) in visit_counts.into_iter().enumerate() {
        //     // if count >
        //     // println!("{:?}", );
        // }
        // let mut count_map: FxHashMap<u32, u32> = FxHashMap();

        // for (index, count) in visit_counts.into_iter().enumerate() {
        //     if count > 6000 {
        //         println!("{:?}", all_nodes[index].data);
        //     }

        //     let count2 = count_map.entry(10 * ((count+9) / 10)).or_insert(0);
        //     *count2 += 1;
        // }

        // let mut count_map: Vec<(u32, u32)> = count_map.into_iter().collect();
        // count_map.sort_by_key(|&(c, _)| c);
        // for (k, v) in count_map.into_iter() {
        //     println!("# of nodes visited at most {:2} times: {:6}", k, v);
        // }

        let mut hashes = FxHashMap();
        for input in inputs.values().flat_map(|v| v.iter().cloned()) {
            hashes.entry(input)
                  .or_insert_with(|| hcx.hash(input).unwrap());
        }

        Predecessors {
            inputs: inputs,
            hashes: hashes,
        }
    }
}
