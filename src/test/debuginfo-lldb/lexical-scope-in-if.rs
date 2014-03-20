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

// BEFORE if
// debugger:print x
// check:[...]$0 = 999
// debugger:print y
// check:[...]$1 = -1
// debugger:continue

// AT BEGINNING of 'then' block
// debugger:print x
// check:[...]$2 = 999
// debugger:print y
// check:[...]$3 = -1
// debugger:continue

// AFTER 1st redeclaration of 'x'
// debugger:print x
// check:[...]$4 = 1001
// debugger:print y
// check:[...]$5 = -1
// debugger:continue

// AFTER 2st redeclaration of 'x'
// debugger:print x
// check:[...]$6 = 1002
// debugger:print y
// check:[...]$7 = 1003
// debugger:continue

// AFTER 1st if expression
// debugger:print x
// check:[...]$8 = 999
// debugger:print y
// check:[...]$9 = -1
// debugger:continue

// BEGINNING of else branch
// debugger:print x
// check:[...]$10 = 999
// debugger:print y
// check:[...]$11 = -1
// debugger:continue

// BEGINNING of else branch
// debugger:print x
// check:[...]$12 = 1004
// debugger:print y
// check:[...]$13 = 1005
// debugger:continue

// BEGINNING of else branch
// debugger:print x
// check:[...]$14 = 999
// debugger:print y
// check:[...]$15 = -1
// debugger:continue

fn main() {

    let x = 999;
    let y = -1;

    zzz(); // #break
    sentinel();

    if x < 1000 {
        zzz(); // #break
        sentinel();

        let x = 1001;

        zzz(); // #break
        sentinel();

        let x = 1002;
        let y = 1003;
        zzz(); // #break
        sentinel();
    } else {
        unreachable!();
    }

    zzz(); // #break
    sentinel();

    if x > 1000 {
        unreachable!();
    } else {
        zzz(); // #break
        sentinel();

        let x = 1004;
        let y = 1005;
        zzz(); // #break
        sentinel();
    }

    zzz(); // #break
    sentinel();
}

fn zzz() {()}
fn sentinel() {()}
