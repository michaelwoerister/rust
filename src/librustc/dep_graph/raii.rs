// Copyright 2012-2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use super::{DepNode};
use super::edges::DepGraphEdges;
use super::graph::CurrentDepGraph;

use std::cell::RefCell;

pub struct DepTask<'graph> {
    graph: &'graph RefCell<DepGraphEdges>,
    graph2: &'graph RefCell<CurrentDepGraph>,
    key: DepNode,
}

impl<'graph> DepTask<'graph> {
    pub(super) fn new(graph: &'graph RefCell<DepGraphEdges>,
               graph2: &'graph RefCell<CurrentDepGraph>,
               key: DepNode)
               -> DepTask<'graph> {
        graph.borrow_mut().push_task(key);
        graph2.borrow_mut().push_task(key);
        DepTask {
            graph,
            graph2,
            key,
        }
    }
}

impl<'graph> Drop for DepTask<'graph> {
    fn drop(&mut self) {
        self.graph.borrow_mut().pop_task(self.key);
        self.graph2.borrow_mut().pop_task(self.key);
    }
}

pub struct IgnoreTask<'graph> {
    graph: &'graph RefCell<DepGraphEdges>,
    graph2: &'graph RefCell<CurrentDepGraph>,
}

impl<'graph> IgnoreTask<'graph> {
    pub(super) fn new(graph: &'graph RefCell<DepGraphEdges>,
               graph2: &'graph RefCell<CurrentDepGraph>)
               -> IgnoreTask<'graph> {
        graph.borrow_mut().push_ignore();
        graph2.borrow_mut().push_ignore();
        IgnoreTask {
            graph,
            graph2,
        }
    }
}

impl<'graph> Drop for IgnoreTask<'graph> {
    fn drop(&mut self) {
        self.graph.borrow_mut().pop_ignore();
        self.graph2.borrow_mut().pop_ignore();
    }
}

