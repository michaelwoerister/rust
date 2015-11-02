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

fn generic_fn<T>(a: T) -> (T, i32) {
    fn nested_fn(a: i32) -> i32 {
        a + 1
    }

    let x = {
        fn nested_fn(a: i32) -> i32 {
            a + 2
        }

        1 + nested_fn(1)
    };

    return (a, x + nested_fn(0));
}

fn main() {

    let _ = generic_fn(0i64);
    let _ = generic_fn(0u16);
    let _ = generic_fn(0i8);
}

//~ CODEGEN_ITEM fn items_within_generic_items::generic_fn[0]<i64>
//~ CODEGEN_ITEM fn items_within_generic_items::generic_fn[0]<u16>
//~ CODEGEN_ITEM fn items_within_generic_items::generic_fn[0]<i8>
//~ CODEGEN_ITEM fn items_within_generic_items::generic_fn[0]::nested_fn[0]
//~ CODEGEN_ITEM fn items_within_generic_items::generic_fn[0]::nested_fn[1]
//~ CODEGEN_ITEM fn items_within_generic_items::main[0]
