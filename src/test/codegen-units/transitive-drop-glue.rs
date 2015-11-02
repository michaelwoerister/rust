// Copyright 2012-2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// ignore-tidy-linelength
#![deny(dead_code)]

//~ CODEGEN_ITEM drop-glue transitive_drop_glue::Root[0]
struct Root(Intermediate);
//~ CODEGEN_ITEM drop-glue transitive_drop_glue::Intermediate[0]
struct Intermediate(Leaf);
//~ CODEGEN_ITEM drop-glue transitive_drop_glue::Leaf[0]
struct Leaf;

impl Drop for Leaf {
    //~ CODEGEN_ITEM fn transitive_drop_glue::Leaf.Drop[0]::drop[0]
    fn drop(&mut self) {}
}

//~ CODEGEN_ITEM drop-glue transitive_drop_glue::Root[0]
struct RootGen<T>(IntermediateGen<T>);
//~ CODEGEN_ITEM drop-glue transitive_drop_glue::Root[0]
struct IntermediateGen<T>(LeafGen<T>);
//~ CODEGEN_ITEM drop-glue transitive_drop_glue::Root[0]
struct LeafGen<T>(T);

impl<T> Drop for LeafGen<T> {
    fn drop(&mut self) {}
}

//~ CODEGEN_ITEM fn transitive_drop_glue::main[0]
fn main() {

    let _ = Root(Intermediate(Leaf));

    //~ CODEGEN_ITEM drop-glue transitive_drop_glue::RootGen[0]<u32>
    //~ CODEGEN_ITEM drop-glue transitive_drop_glue::IntermediateGen[0]<u32>
    //~ CODEGEN_ITEM drop-glue transitive_drop_glue::LeafGen[0]<u32>
    //~ CODEGEN_ITEM fn transitive_drop_glue::LeafGen<T>.Drop[0]::drop[0]<u32>
    let _ = RootGen(IntermediateGen(LeafGen(0u32)));

    //~ CODEGEN_ITEM drop-glue transitive_drop_glue::RootGen[0]<i16>
    //~ CODEGEN_ITEM drop-glue transitive_drop_glue::IntermediateGen[0]<i16>
    //~ CODEGEN_ITEM drop-glue transitive_drop_glue::LeafGen[0]<i16>
    //~ CODEGEN_ITEM fn transitive_drop_glue::LeafGen<T>.Drop[0]::drop[0]<i16>
    let _ = RootGen(IntermediateGen(LeafGen(0i16)));
}
