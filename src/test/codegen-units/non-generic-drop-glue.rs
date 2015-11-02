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
// compile-flags:-Zprint-codegen-items=eager

#![deny(dead_code)]

struct StructWithDrop {
    x: i32
}

impl Drop for StructWithDrop {
    fn drop(&mut self) {}
}

struct StructNoDrop {
    x: i32
}

enum EnumWithDrop {
    A(i32)
}

impl Drop for EnumWithDrop {
    fn drop(&mut self) {}
}

enum EnumNoDrop {
    A(i32)
}


fn main() {
    let _ = StructWithDrop { x: 0 }.x;
    let _ = StructNoDrop { x: 0 }.x;
    let _ = match EnumWithDrop::A(0) {
        EnumWithDrop::A(x) => x
    };
    let _ = match EnumNoDrop::A(0) {
        EnumNoDrop::A(x) => x
    };
}

//~ CODEGEN_ITEM drop-glue non_generic_drop_glue::StructWithDrop[0]
//~ CODEGEN_ITEM fn non_generic_drop_glue::StructWithDrop.Drop[0]::drop[0]

//~ CODEGEN_ITEM drop-glue non_generic_drop_glue::EnumWithDrop[0]
//~ CODEGEN_ITEM fn non_generic_drop_glue::EnumWithDrop.Drop[0]::drop[0]

//~ CODEGEN_ITEM fn non_generic_drop_glue::main[0]

//~ CODEGEN_ITEM drop-glue i8
