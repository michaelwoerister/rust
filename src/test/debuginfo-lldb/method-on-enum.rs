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

// STACK BY REF
// debugger:print *self
// check:[...]$0 = {{Variant2, [...]}, {Variant2, 117901063}}
// debugger:print arg1
// check:[...]$1 = -1
// debugger:print arg2
// check:[...]$2 = -2
// debugger:continue

// STACK BY VAL
// debugger:print self
// check:[...]$3 = {{Variant2, [...]}, {Variant2, 117901063}}
// debugger:print arg1
// check:[...]$4 = -3
// debugger:print arg2
// check:[...]$5 = -4
// debugger:continue

// OWNED BY REF
// debugger:print *self
// check:[...]$6 = {{Variant1, x = 1799, y = 1799}, {Variant1, [...]}}
// debugger:print arg1
// check:[...]$7 = -5
// debugger:print arg2
// check:[...]$8 = -6
// debugger:continue

// OWNED BY VAL
// debugger:print self
// check:[...]$9 = {{Variant1, x = 1799, y = 1799}, {Variant1, [...]}}
// debugger:print arg1
// check:[...]$10 = -7
// debugger:print arg2
// check:[...]$11 = -8
// debugger:continue

// OWNED MOVED
// debugger:print *self
// check:[...]$12 = {{Variant1, x = 1799, y = 1799}, {Variant1, [...]}}
// debugger:print arg1
// check:[...]$13 = -9
// debugger:print arg2
// check:[...]$14 = -10
// debugger:continue

#[feature(struct_variant)];

enum Enum {
    Variant1 { x: u16, y: u16 },
    Variant2 (u32)
}

impl Enum {

    fn self_by_ref(&self, arg1: int, arg2: int) -> int {
        arg1 + arg2 // #break
    }

    fn self_by_val(self, arg1: int, arg2: int) -> int {
        arg1 + arg2 // #break
    }

    fn self_owned(~self, arg1: int, arg2: int) -> int {
        arg1 + arg2 // #break
    }
}

fn main() {
    let stack = Variant2(117901063);
    let _ = stack.self_by_ref(-1, -2);
    let _ = stack.self_by_val(-3, -4);

    let owned = ~Variant1{ x: 1799, y: 1799 };
    let _ = owned.self_by_ref(-5, -6);
    let _ = owned.self_by_val(-7, -8);
    let _ = owned.self_owned(-9, -10);
}
