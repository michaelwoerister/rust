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

// FIRST ITERATION
// debugger:print x
// check:[...]$0 = 0
// debugger:continue

// debugger:print x
// check:[...]$1 = 1
// debugger:continue

// debugger:print x
// check:[...]$2 = 101
// debugger:continue

// debugger:print x
// check:[...]$3 = 101
// debugger:continue

// debugger:print x
// check:[...]$4 = -987
// debugger:continue

// debugger:print x
// check:[...]$5 = 101
// debugger:continue


// SECOND ITERATION
// debugger:print x
// check:[...]$6 = 1
// debugger:continue

// debugger:print x
// check:[...]$7 = 2
// debugger:continue

// debugger:print x
// check:[...]$8 = 102
// debugger:continue

// debugger:print x
// check:[...]$9 = 102
// debugger:continue

// debugger:print x
// check:[...]$10 = -987
// debugger:continue

// debugger:print x
// check:[...]$11 = 102
// debugger:continue

// debugger:print x
// check:[...]$12 = 2
// debugger:continue

fn main() {

    let mut x = 0;

    loop {
        if x >= 2 {
            break;
        }

        zzz(); // #break
        sentinel();

        x += 1;
        zzz(); // #break
        sentinel();

        // Shadow x
        let x = x + 100;
        zzz(); // #break
        sentinel();

        // open scope within loop's top level scope
        {
            zzz(); // #break
            sentinel();

            let x = -987;

            zzz(); // #break
            sentinel();
        }

        // Check that we get the x before the inner scope again
        zzz(); // #break
        sentinel();
    }

    zzz(); // #break
    sentinel();
}

fn zzz() {()}
fn sentinel() {()}
