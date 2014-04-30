// Copyright 2013-2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// ignore-android: FIXME(#10381)

// compile-flags:-g

// === GDB TESTS ===================================================================================

// gdb-command:rbreak zzz
// gdb-command:run

// STACK BY REF
// gdb-command:finish
// gdb-command:print *self
// gdb-check:$1 = {100, -100.5}
// gdb-command:print arg1
// gdb-check:$2 = -1
// gdb-command:print arg2
// gdb-check:$3 = -2
// gdb-command:continue

// STACK BY VAL
// gdb-command:finish
// gdb-command:print self
// gdb-check:$4 = {100, -100.5}
// gdb-command:print arg1
// gdb-check:$5 = -3
// gdb-command:print arg2
// gdb-check:$6 = -4
// gdb-command:continue

// OWNED BY REF
// gdb-command:finish
// gdb-command:print *self
// gdb-check:$7 = {200, -200.5}
// gdb-command:print arg1
// gdb-check:$8 = -5
// gdb-command:print arg2
// gdb-check:$9 = -6
// gdb-command:continue

// OWNED BY VAL
// gdb-command:finish
// gdb-command:print self
// gdb-check:$10 = {200, -200.5}
// gdb-command:print arg1
// gdb-check:$11 = -7
// gdb-command:print arg2
// gdb-check:$12 = -8
// gdb-command:continue

// OWNED MOVED
// gdb-command:finish
// gdb-command:print *self
// gdb-check:$13 = {200, -200.5}
// gdb-command:print arg1
// gdb-check:$14 = -9
// gdb-command:print arg2
// gdb-check:$15 = -10
// gdb-command:continue


// === LLDB TESTS ==================================================================================

// lldb-command:run

// STACK BY REF
// lldb-command:print *self
// lldb-lldb-check:[...]$0 = TupleStruct(100, -100.5)
// lldb-command:print arg1
// lldb-lldb-check:[...]$1 = -1
// lldb-command:print arg2
// lldb-lldb-check:[...]$2 = -2
// lldb-command:continue

// STACK BY VAL
// lldb-command:print self
// lldb-lldb-check:[...]$3 = TupleStruct(100, -100.5)
// lldb-command:print arg1
// lldb-lldb-check:[...]$4 = -3
// lldb-command:print arg2
// lldb-lldb-check:[...]$5 = -4
// lldb-command:continue

// OWNED BY REF
// lldb-command:print *self
// lldb-lldb-check:[...]$6 = TupleStruct(200, -200.5)
// lldb-command:print arg1
// lldb-lldb-check:[...]$7 = -5
// lldb-command:print arg2
// lldb-lldb-check:[...]$8 = -6
// lldb-command:continue

// OWNED BY VAL
// lldb-command:print self
// lldb-lldb-check:[...]$9 = TupleStruct(200, -200.5)
// lldb-command:print arg1
// lldb-lldb-check:[...]$10 = -7
// lldb-command:print arg2
// lldb-lldb-check:[...]$11 = -8
// lldb-command:continue

// OWNED MOVED
// lldb-command:print *self
// lldb-lldb-check:[...]$12 = TupleStruct(200, -200.5)
// lldb-command:print arg1
// lldb-lldb-check:[...]$13 = -9
// lldb-command:print arg2
// lldb-lldb-check:[...]$14 = -10
// lldb-command:continue

struct TupleStruct(int, f64);

impl TupleStruct {

    fn self_by_ref(&self, arg1: int, arg2: int) -> int {
        zzz(); // #break
        arg1 + arg2
    }

    fn self_by_val(self, arg1: int, arg2: int) -> int {
        zzz(); // #break
        arg1 + arg2
    }

    fn self_owned(~self, arg1: int, arg2: int) -> int {
        zzz(); // #break
        arg1 + arg2
    }
}

fn main() {
    let stack = TupleStruct(100, -100.5);
    let _ = stack.self_by_ref(-1, -2);
    let _ = stack.self_by_val(-3, -4);

    let owned = box TupleStruct(200, -200.5);
    let _ = owned.self_by_ref(-5, -6);
    let _ = owned.self_by_val(-7, -8);
    let _ = owned.self_owned(-9, -10);
}

fn zzz() {()}
