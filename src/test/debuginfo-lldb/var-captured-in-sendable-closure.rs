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

// debugger:print constant
// check:[...]$0 = 1
// debugger:print a_struct
// check:[...]$1 = (a = -2, b = 3.5, c = 4)
// debugger:print *owned
// check:[...]$2 = 5

#[allow(unused_variable)];

struct Struct {
    a: int,
    b: f64,
    c: uint
}

fn main() {
    let constant = 1;

    let a_struct = Struct {
        a: -2,
        b: 3.5,
        c: 4
    };

    let owned = ~5;

    let closure: proc() = proc() {
        do_something(&constant, &a_struct.a, owned); // #break
    };

    closure();
}

fn do_something(_: &int, _:&int, _:&int) {

}
