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
// debugger:print b
// check:[...]$0 = false
// debugger:print i
// check:[...]$1 = -1

// NOTE: LLDB does not support 32bit chars
// d ebugger:print (uint)(c)
// c heck:$3 = 97

// debugger:print/d i8
// check:[...]$2 = 68
// debugger:print i16
// check:[...]$3 = -16
// debugger:print i32
// check:[...]$4 = -32
// debugger:print i64
// check:[...]$5 = -64
// debugger:print u
// check:[...]$6 = 1
// debugger:print/d u8
// check:[...]$7 = 100
// debugger:print u16
// check:[...]$8 = 16
// debugger:print u32
// check:[...]$9 = 32
// debugger:print u64
// check:[...]$10 = 64
// debugger:print f32
// check:[...]$11 = 2.5
// debugger:print f64
// check:[...]$12 = 3.5

#[allow(unused_variable)];

fn main() {
    let b: bool = false;
    let i: int = -1;
    let c: char = 'a';
    let i8: i8 = 68;
    let i16: i16 = -16;
    let i32: i32 = -32;
    let i64: i64 = -64;
    let u: uint = 1;
    let u8: u8 = 100;
    let u16: u16 = 16;
    let u32: u32 = 32;
    let u64: u64 = 64;
    let f32: f32 = 2.5;
    let f64: f64 = 3.5;

    (); // #break
}
