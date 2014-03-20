// Copyright 2013-2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// LLDB multiline issues
// ignore-test

// compile-flags:-g
// debugger:run

// debugger:print *t0
// check:[...]$0 = 1
// debugger:print *t1
// check:[...]$1 = 2.5
// debugger:print ret
// check:[...]$2 = ((1, 2.5), (2.5, 1))
// debugger:continue

// debugger:print *t0
// check:[...]$3 = 3.5
// debugger:print *t1
// check:[...]$4 = 4
// debugger:print ret
// check:[...]$5 = ((3.5, 4), (4, 3.5))
// debugger:continue

// debugger:print *t0
// check:[...]$6 = 5
// debugger:print *t1
// check:[...]$7 = (a = 6, b = 7.5)
// debugger:print ret
// check:[...]$8 = ((5, (a = 6, b = 7.5)), ((a = 6, b = 7.5), 5))
// debugger:continue

#[deriving(Clone)]
struct Struct {
    a: int,
    b: f64
}

fn dup_tup<T0: Clone, T1: Clone>(t0: &T0, t1: &T1) -> ((T0, T1), (T1, T0)) {
    let ret = ((t0.clone(), t1.clone()), (t1.clone(), t0.clone()));
    ret // #break
}

fn main() {

    let _ = dup_tup(&1, &2.5);
    let _ = dup_tup(&3.5, &4_u16);
    let _ = dup_tup(&5, &Struct { a: 6, b: 7.5 });
}
