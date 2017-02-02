// Copyright 2012-2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use rustc::hir::intravisit::{Visitor, NestedVisitorMap};

// use encoder::EncodeContext;
use schema::*;

use rustc::hir;
use rustc::ty;

use rustc_serialize::{Encoder, Encodable};

#[derive(RustcEncodable, RustcDecodable)]
pub struct Ast<'tcx> {
    pub body: Lazy<hir::Body>,
    pub tables: Lazy<ty::Tables<'tcx>>,
    pub nested_bodies: LazySeq<hir::Body>,
    pub rvalue_promotable_to_static: bool,
}

impl<'tcx> ::std::fmt::Debug for Ast<'tcx> {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "astencode::Ast")
    }
}

// impl<'a, 'tcx> EncodeContext<'a, 'tcx> {
impl<'a, 'b: 'a, 'tcx: 'b> ::index_builder::EntryBuilder<'a, 'b, 'tcx> {
    pub fn encode_body(&mut self, body_id: hir::BodyId) -> Lazy<Ast<'tcx>> {
        let body = self.tcx.map.body(body_id);
        let lazy_body = self.lazy(body);

        let tables = self.tcx.body_tables(body_id);
        let lazy_tables = self.lazy(tables);
        let tcx = self.tcx;
        let nested_pos = self.encoder().position();
        let nested_count = {
            let mut visitor = NestedBodyEncodingVisitor {
                encoder: self.encoder(),
                tcx: tcx,
                count: 0,
            };
            visitor.visit_body(body);
            visitor.count
        };

        let rvalue_promotable_to_static =
            self.tcx.rvalue_promotable_to_static.borrow()[&body.value.id];

        self.lazy(&Ast {
            body: lazy_body,
            tables: lazy_tables,
            nested_bodies: LazySeq::with_position_and_length(nested_pos, nested_count),
            rvalue_promotable_to_static: rvalue_promotable_to_static
        })
    }
}

struct NestedBodyEncodingVisitor<'a, 'b, 'tcx: 'b, ERR, E>
    where E: Encoder<Error = ERR> + 'a,
          ERR: ::std::fmt::Debug,
{
    encoder: &'a mut E,
    tcx: ty::TyCtxt<'b, 'tcx, 'tcx>,
    count: usize,
}

impl<'a, 'b, 'tcx, ERR, E> Visitor<'tcx> for NestedBodyEncodingVisitor<'a, 'b, 'tcx, ERR, E>
    where E: Encoder<Error = ERR> + 'a,
          ERR: ::std::fmt::Debug,
{
    fn nested_visit_map<'this>(&'this mut self) -> NestedVisitorMap<'this, 'tcx> {
        NestedVisitorMap::None
    }

    fn visit_nested_body(&mut self, body: hir::BodyId) {
        let body = self.tcx.map.body(body);
        body.encode(self.encoder).unwrap();
        self.count += 1;

        self.visit_body(body);
    }
}
