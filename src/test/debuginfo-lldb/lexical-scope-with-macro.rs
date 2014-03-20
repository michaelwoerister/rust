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
// check:[...]$0 = 10
// debugger:print b
// check:[...]$1 = 34
// debugger:continue

// debugger:print a
// check:[...]$2 = 890242
// debugger:print b
// check:[...]$3 = 34
// debugger:continue

// debugger:print a
// check:[...]$4 = 10
// debugger:print b
// check:[...]$5 = 34
// debugger:continue

// debugger:print a
// check:[...]$6 = 102
// debugger:print b
// check:[...]$7 = 34
// debugger:continue

// debugger:print a
// check:[...]$8 = 110
// debugger:print b
// check:[...]$9 = 34
// debugger:continue

// debugger:print a
// check:[...]$10 = 10
// debugger:print b
// check:[...]$11 = 34
// debugger:continue

// debugger:print a
// check:[...]$12 = 10
// debugger:print b
// check:[...]$13 = 34
// debugger:print c
// check:[...]$14 = 400
// debugger:continue

#[feature(macro_rules)];

macro_rules! trivial(
    ($e1:expr) => ($e1)
)

macro_rules! no_new_scope(
    ($e1:expr) => (($e1 + 2) - 1)
)

macro_rules! new_scope(
    () => ({
        let a = 890242;
        zzz(); // #break
        sentinel();
    })
)

macro_rules! shadow_within_macro(
    ($e1:expr) => ({
        let a = $e1 + 2;

        zzz(); // #break
        sentinel();

        let a = $e1 + 10;

        zzz(); // #break
        sentinel();
    })
)


macro_rules! dup_expr(
    ($e1:expr) => (($e1) + ($e1))
)


fn main() {

    let a = trivial!(10);
    let b = no_new_scope!(33);

    zzz(); // #break
    sentinel();

    new_scope!();

    zzz(); // #break
    sentinel();

    shadow_within_macro!(100);

    zzz(); // #break
    sentinel();

    let c = dup_expr!(10 * 20);

    zzz(); // #break
    sentinel();
}

fn zzz() {()}
fn sentinel() {()}
