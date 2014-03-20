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

// debugger:print a
// check:[...]$0 = 10101
// debugger:continue

// debugger:print b
// check:[...]$1 = 20202
// debugger:continue

// debugger:print c
// check:[...]$2 = 30303

#[allow(unused_variable)];

fn function_one() {
    let a = 10101;
    (); // #break
}

fn function_two() {
    let b = 20202;
    (); // #break
}


fn function_three() {
    let c = 30303;
    (); // #break
}


fn main() {
    function_one();
    function_two();
    function_three();
}
