// Copyright 2013-2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#![feature(managed_boxes)];
#![allow(unused_variable)];

// compile-flags:-g
// debugger:run

// debugger:print unique->fill
// check:[...]$0 = 32
// debugger:print *(((uint64_t*)unique->elements) + 0)
// check:[...]$1 = 10
// debugger:print *(((uint64_t*)unique->elements) + 1)
// check:[...]$2 = 11
// debugger:print *(((uint64_t*)unique->elements) + 2)
// check:[...]$3 = 12
// debugger:print *(((uint64_t*)unique->elements) + 3)
// check:[...]$4 = 13

fn main() {

    let unique: ~[i64] = ~[10, 11, 12, 13];

    zzz(); // #break
}

fn zzz() { () }
