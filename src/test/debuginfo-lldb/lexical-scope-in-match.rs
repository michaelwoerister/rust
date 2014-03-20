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

// debugger:print shadowed
// check:[...]$0 = 231
// debugger:print not_shadowed
// check:[...]$1 = 232
// debugger:continue

// debugger:print shadowed
// check:[...]$2 = 233
// debugger:print not_shadowed
// check:[...]$3 = 232
// debugger:print local_to_arm
// check:[...]$4 = 234
// debugger:continue

// debugger:print shadowed
// check:[...]$5 = 236
// debugger:print not_shadowed
// check:[...]$6 = 232
// debugger:continue

// debugger:print shadowed
// check:[...]$7 = 237
// debugger:print not_shadowed
// check:[...]$8 = 232
// debugger:print local_to_arm
// check:[...]$9 = 238
// debugger:continue

// debugger:print shadowed
// check:[...]$10 = 239
// debugger:print not_shadowed
// check:[...]$11 = 232
// debugger:continue

// debugger:print shadowed
// check:[...]$12 = 241
// debugger:print not_shadowed
// check:[...]$13 = 232
// debugger:continue

// debugger:print shadowed
// check:[...]$14 = 243
// debugger:print *local_to_arm
// check:[...]$15 = 244
// debugger:continue

// debugger:print shadowed
// check:[...]$16 = 231
// debugger:print not_shadowed
// check:[...]$17 = 232
// debugger:continue

struct Struct {
    x: int,
    y: int
}

fn main() {

    let shadowed = 231;
    let not_shadowed = 232;

    zzz(); // #break
    sentinel();

    match (233, 234) {
        (shadowed, local_to_arm) => {

            zzz(); // #break
            sentinel();
        }
    }

    match (235, 236) {
        // with literal
        (235, shadowed) => {

            zzz(); // #break
            sentinel();
        }
        _ => {}
    }

    match Struct { x: 237, y: 238 } {
        Struct { x: shadowed, y: local_to_arm } => {

            zzz(); // #break
            sentinel();
        }
    }

    match Struct { x: 239, y: 240 } {
        // ignored field
        Struct { x: shadowed, .. } => {

            zzz(); // #break
            sentinel();
        }
    }

    match Struct { x: 241, y: 242 } {
        // with literal
        Struct { x: shadowed, y: 242 } => {

            zzz(); // #break
            sentinel();
        }
        _ => {}
    }

    match (243, 244) {
        (shadowed, ref local_to_arm) => {

            zzz(); // #break
            sentinel();
        }
    }

    zzz(); // #break
    sentinel();
}

fn zzz() {()}
fn sentinel() {()}
