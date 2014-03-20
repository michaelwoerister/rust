// Copyright 2013-2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// compile-flags:-g
// debugger:run

// debugger:print x
// check:[...]$0 = 0.5
// debugger:print y
// check:[...]$1 = 10
// debugger:continue

// debugger:print *x
// check:[...]$2 = 29
// debugger:print *y
// check:[...]$3 = 110
// debugger:continue

fn some_generic_fun<T1, T2>(a: T1, b: T2) -> (T2, T1) {

    let closure = |x, y| {
        (y, x) // #break
    };

    closure(a, b)
}

fn main() {
    some_generic_fun(0.5, 10);
    some_generic_fun(&29, ~110);
}
