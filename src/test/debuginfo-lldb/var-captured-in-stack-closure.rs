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

// debugger:print variable
// check:[...]$0 = 1
// debugger:print constant
// check:[...]$1 = 2
// debugger:print a_struct
// check:[...]$2 = (a = -3, b = 4.5, c = 5)
// debugger:print *struct_ref
// check:[...]$3 = (a = -3, b = 4.5, c = 5)
// debugger:print *owned
// check:[...]$4 = 6
// debugger:print managed->val
// check:[...]$5 = 7

#[feature(managed_boxes)];
#[allow(unused_variable)];

struct Struct {
    a: int,
    b: f64,
    c: uint
}

fn main() {
    let mut variable = 1;
    let constant = 2;

    let a_struct = Struct {
        a: -3,
        b: 4.5,
        c: 5
    };

    let struct_ref = &a_struct;
    let owned = ~6;
    let managed = @7;

    let closure = || {
        variable = constant + a_struct.a + struct_ref.a + *owned + *managed; // #break
    };

    closure();
}
