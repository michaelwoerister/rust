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

fn foo1<T1>(a: T1) -> (T1, u32) {
    (a, 1)
}

fn foo2<T1, T2>(a: T1, b: T2) -> (T1, T2) {
    (a, b)
}

fn foo3<T1, T2, T3>(a: T1, b: T2, c: T3) -> (T1, T2, T3) {
    (a, b, c)
}

fn main() {
    let _ = foo1(2i32);
    let _ = foo1(2i64);
    let _ = foo1("abc");
    let _ = foo1('v');

    let _ = foo2(2i32, 2i32);
    let _ = foo2(2i64, "abc");
    let _ = foo2("a", 2usize);
    let _ = foo2('v', ());

    let _ = foo3(2i32, 2i32, 2i32);
    let _ = foo3(2i64, "abc", 'c');
    let _ = foo3(0i16, "a", 2usize);
    let _ = foo3('v', (), ());
}

//~ CODEGEN_ITEM fn generic_functions::foo1[0]<i32>
//~ CODEGEN_ITEM fn generic_functions::foo1[0]<i64>
//~ CODEGEN_ITEM fn generic_functions::foo1[0]<&str>
//~ CODEGEN_ITEM fn generic_functions::foo1[0]<char>

//~ CODEGEN_ITEM fn generic_functions::foo2[0]<i32, i32>
//~ CODEGEN_ITEM fn generic_functions::foo2[0]<i64, &str>
//~ CODEGEN_ITEM fn generic_functions::foo2[0]<&str, usize>
//~ CODEGEN_ITEM fn generic_functions::foo2[0]<char, ()>

//~ CODEGEN_ITEM fn generic_functions::foo3[0]<i32, i32, i32>
//~ CODEGEN_ITEM fn generic_functions::foo3[0]<i64, &str, char>
//~ CODEGEN_ITEM fn generic_functions::foo3[0]<i16, &str, usize>
//~ CODEGEN_ITEM fn generic_functions::foo3[0]<char, (), ()>

//~ CODEGEN_ITEM fn generic_functions::main[0]
