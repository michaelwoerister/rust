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

fn take_foo<T1, T2, F: FnOnce(T1, T2)>(f: F, x: T1, y: T2) {
    (f)(x, y)
}

fn foo<T1, T2>(_: T1, _: T2) {}

//~ CODEGEN_ITEM fn function_as_argument::main[0]
fn main() {

    //~ CODEGEN_ITEM fn function_as_argument::take_foo[0]<u32, &str, fn(u32, &str)>
    //~ CODEGEN_ITEM fn function_as_argument::foo[0]<u32, &str>
    take_foo(foo, 0u32, "abc");

    //~ CODEGEN_ITEM fn function_as_argument::take_foo[0]<char, f64, fn(char, f64)>
    //~ CODEGEN_ITEM fn function_as_argument::foo[0]<char, f64>
    take_foo(foo, 'c', 0f64);
}

