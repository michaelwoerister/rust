// Copyright 2016 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// ignore-tidy-linelength
// compile-flags:-Zprint-trans-items=eager

#![allow(dead_code)]
#![crate_type="lib"]

//~ TRANS_ITEM drop-glue local_drop_glue::Struct[0] @@ local_drop_glue[E]
struct Struct {
    _a: u32
}

impl Drop for Struct {
    //~ TRANS_ITEM fn local_drop_glue::{{impl}}[0]::drop[0] @@ local_drop_glue[E]
    fn drop(&mut self) {}
}

//~ TRANS_ITEM drop-glue local_drop_glue::Outer[0] @@ local_drop_glue[E]
struct Outer {
    _a: Struct
}

mod mod1
{
    use super::Struct;

    //~ TRANS_ITEM drop-glue local_drop_glue::mod1[0]::Struct2[0] @@ local_drop_glue-mod1[E]
    struct Struct2 {
        _a: Struct,
        //~ TRANS_ITEM drop-glue (u32, local_drop_glue::Struct[0]) @@ local_drop_glue[E]
        _b: (u32, Struct),
    }
}
