// Copyright 2013-2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#[feature(managed_boxes)];

// compile-flags:-g
// debugger:run

// debugger:print *ordinary_unique
// check:[...]$0 = (-1, -2)

// debugger:print managed_within_unique->x
// check:[...]$1 = -3

// debugger:print managed_within_unique->y->val
// check:[...]$2 = -4

#[allow(unused_variable)];

struct ContainsManaged {
    x: int,
    y: @int
}

fn main() {
    let ordinary_unique = ~(-1, -2);

    let managed_within_unique = ~ContainsManaged { x: -3, y: @-4 };

    (); // #break
}
