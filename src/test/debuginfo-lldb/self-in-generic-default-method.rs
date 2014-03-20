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

// STACK BY REF
// debugger:print *self
// check:[...]$0 = (x = 987)
// debugger:print arg1
// check:[...]$1 = -1
// debugger:print/d arg2
// check:[...]$2 = -2
// debugger:continue

// STACK BY VAL
// debugger:print self
// check:[...]$3 = (x = 987)
// debugger:print arg1
// check:[...]$4 = -3
// debugger:print arg2
// check:[...]$5 = -4
// debugger:continue

// OWNED BY REF
// debugger:print *self
// check:[...]$6 = (x = 879)
// debugger:print arg1
// check:[...]$7 = -5
// debugger:print arg2
// check:[...]$8 = -6
// debugger:continue

// OWNED BY VAL
// debugger:print self
// check:[...]$9 = (x = 879)
// debugger:print arg1
// check:[...]$10 = -7
// debugger:print arg2
// check:[...]$11 = -8
// debugger:continue

// OWNED MOVED
// debugger:print *self
// check:[...]$12 = (x = 879)
// debugger:print arg1
// check:[...]$13 = -9
// debugger:print arg2
// check:[...]$14 = -10.5
// debugger:continue

struct Struct {
    x: int
}

trait Trait {

    fn self_by_ref<T>(&self, arg1: int, arg2: T) -> int {
        arg1 // #break
    }

    fn self_by_val<T>(self, arg1: int, arg2: T) -> int {
        arg1 // #break
    }

    fn self_owned<T>(~self, arg1: int, arg2: T) -> int {
        arg1 // #break
    }
}

impl Trait for Struct {}

fn main() {
    let stack = Struct { x: 987 };
    let _ = stack.self_by_ref(-1, -2_i8);
    let _ = stack.self_by_val(-3, -4_i16);

    let owned = ~Struct { x: 879 };
    let _ = owned.self_by_ref(-5, -6_i32);
    let _ = owned.self_by_val(-7, -8_i64);
    let _ = owned.self_owned(-9, -10.5_f32);
}
