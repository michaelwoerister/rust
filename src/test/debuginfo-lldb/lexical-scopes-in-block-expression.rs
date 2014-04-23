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

// STRUCT EXPRESSION
// debugger:print val
// check:[...]$0 = -1
// debugger:print ten
// check:[...]$1 = 10
// debugger:continue

// debugger:print val
// check:[...]$2 = 11
// debugger:print ten
// check:[...]$3 = 10
// debugger:continue

// debugger:print val
// check:[...]$4 = -1
// debugger:print ten
// check:[...]$5 = 10
// debugger:continue

// FUNCTION CALL
// debugger:print val
// check:[...]$6 = -1
// debugger:print ten
// check:[...]$7 = 10
// debugger:continue

// debugger:print val
// check:[...]$8 = 12
// debugger:print ten
// check:[...]$9 = 10
// debugger:continue

// debugger:print val
// check:[...]$10 = -1
// debugger:print ten
// check:[...]$11 = 10
// debugger:continue

// TUPLE EXPRESSION
// debugger:print val
// check:[...]$12 = -1
// debugger:print ten
// check:[...]$13 = 10
// debugger:continue

// debugger:print val
// check:[...]$14 = 13
// debugger:print ten
// check:[...]$15 = 10
// debugger:continue

// debugger:print val
// check:[...]$16 = -1
// debugger:print ten
// check:[...]$17 = 10
// debugger:continue

// VEC EXPRESSION
// debugger:print val
// check:[...]$18 = -1
// debugger:print ten
// check:[...]$19 = 10
// debugger:continue

// debugger:print val
// check:[...]$20 = 14
// debugger:print ten
// check:[...]$21 = 10
// debugger:continue

// debugger:print val
// check:[...]$22 = -1
// debugger:print ten
// check:[...]$23 = 10
// debugger:continue

// REPEAT VEC EXPRESSION
// debugger:print val
// check:[...]$24 = -1
// debugger:print ten
// check:[...]$25 = 10
// debugger:continue

// debugger:print val
// check:[...]$26 = 15
// debugger:print ten
// check:[...]$27 = 10
// debugger:continue

// debugger:print val
// check:[...]$28 = -1
// debugger:print ten
// check:[...]$29 = 10
// debugger:continue

// ASSIGNMENT EXPRESSION
// debugger:print val
// check:[...]$30 = -1
// debugger:print ten
// check:[...]$31 = 10
// debugger:continue

// debugger:print val
// check:[...]$32 = 16
// debugger:print ten
// check:[...]$33 = 10
// debugger:continue

// debugger:print val
// check:[...]$34 = -1
// debugger:print ten
// check:[...]$35 = 10
// debugger:continue


// ARITHMETIC EXPRESSION
// debugger:print val
// check:[...]$36 = -1
// debugger:print ten
// check:[...]$37 = 10
// debugger:continue

// debugger:print val
// check:[...]$38 = 17
// debugger:print ten
// check:[...]$39 = 10
// debugger:continue

// debugger:print val
// check:[...]$40 = -1
// debugger:print ten
// check:[...]$41 = 10
// debugger:continue

// INDEX EXPRESSION
// debugger:print val
// check:[...]$42 = -1
// debugger:print ten
// check:[...]$43 = 10
// debugger:continue

// debugger:print val
// check:[...]$44 = 18
// debugger:print ten
// check:[...]$45 = 10
// debugger:continue

// debugger:print val
// check:[...]$46 = -1
// debugger:print ten
// check:[...]$47 = 10
// debugger:continue

struct Point {
    x: int,
    y: int
}

fn a_function(x: int) -> int {
    x + 1
}

fn main() {

    let val = -1;
    let ten = 10;

    // surrounded by struct expression
    let point = Point {
        x: {
            zzz(); // #break
            sentinel();

            let val = ten + 1;

            zzz(); // #break
            sentinel();

            val
        },
        y: 10
    };

    zzz(); // #break
    sentinel();

    // surrounded by function call
    let _ = a_function({
        zzz(); // #break
        sentinel();

        let val = ten + 2;

        zzz(); // #break
        sentinel();

        val
    });

    zzz(); // #break
    sentinel();


    // surrounded by tup
    let _ = ({
        zzz(); // #break
        sentinel();

        let val = ten + 3;

        zzz(); // #break
        sentinel();

        val
    }, 0);

    zzz(); // #break
    sentinel();

    // surrounded by vec
    let _ = [{
        zzz(); // #break
        sentinel();

        let val = ten + 4;

        zzz(); // #break
        sentinel();

        val
    }, 0, 0];

    zzz(); // #break
    sentinel();

    // surrounded by repeat vec
    let _ = [{
        zzz(); // #break
        sentinel();

        let val = ten + 5;

        zzz(); // #break
        sentinel();

        val
    }, ..10];

    zzz(); // #break
    sentinel();

    // assignment expression
    let mut var = 0;
    var = {
        zzz(); // #break
        sentinel();

        let val = ten + 6;

        zzz(); // #break
        sentinel();

        val
    };

    zzz(); // #break
    sentinel();

    // arithmetic expression
    var = 10 + -{
        zzz(); // #break
        sentinel();

        let val = ten + 7;

        zzz(); // #break
        sentinel();

        val
    } * 5;

    zzz(); // #break
    sentinel();

    // index expression
    let a_vector = [10, ..20];
    let _ = a_vector[{
        zzz(); // #break
        sentinel();

        let val = ten + 8;

        zzz(); // #break
        sentinel();

        val as uint
    }];

    zzz(); // #break
    sentinel();
}

fn zzz() {()}
fn sentinel() {()}
