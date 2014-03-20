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

// debugger:print x
// check:[...]$0 = false
// debugger:continue

// debugger:print x
// check:[...]$1 = false
// debugger:continue

// debugger:print x
// check:[...]$2 = 10
// debugger:continue

// debugger:print x
// check:[...]$3 = 10
// debugger:continue

// debugger:print x
// check:[...]$4 = 10.5
// debugger:continue

// debugger:print x
// check:[...]$5 = 10
// debugger:continue

// debugger:print x
// check:[...]$6 = false
// debugger:continue


fn main() {
    let x = false;

    zzz(); // #break
    sentinel();

    {
        zzz(); // #break
        sentinel();

        let x = 10;

        zzz(); // #break
        sentinel();

        {
            zzz(); // #break
            sentinel();

            let x = 10.5;

            zzz(); // #break
            sentinel();
        }

        zzz(); // #break
        sentinel();
    }

    zzz(); // #break
    sentinel();
}

fn zzz() {()}
fn sentinel() {()}
