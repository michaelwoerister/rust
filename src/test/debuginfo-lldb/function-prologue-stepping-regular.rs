// Copyright 2013-2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// This test case checks if function arguments already have the correct value when breaking at the
// beginning of a function.

// compile-flags:-g
// debugger:breakpoint set --name immediate_args
// debugger:breakpoint set --name non_immediate_args
// debugger:breakpoint set --name binding
// debugger:breakpoint set --name assignment
// debugger:breakpoint set --name function_call
// debugger:breakpoint set --name identifier
// debugger:breakpoint set --name return_expr
// debugger:breakpoint set --name arithmetic_expr
// debugger:breakpoint set --name if_expr
// debugger:breakpoint set --name while_expr
// debugger:breakpoint set --name loop_expr
// debugger:run

// IMMEDIATE ARGS
// debugger:print a
// check:[...]$0 = 1
// debugger:print b
// check:[...]$1 = true
// debugger:print c
// check:[...]$2 = 2.5
// debugger:continue

// NON IMMEDIATE ARGS
// debugger:print a
// check:[...]$3 = (a = 3, b = 4, c = 5, d = 6, e = 7, f = 8, g = 9, h = 10)
// debugger:print b
// check:[...]$4 = (a = 11, b = 12, c = 13, d = 14, e = 15, f = 16, g = 17, h = 18)
// debugger:continue

// BINDING
// debugger:print a
// check:[...]$5 = 19
// debugger:print b
// check:[...]$6 = 20
// debugger:print c
// check:[...]$7 = 21.5
// debugger:continue

// ASSIGNMENT
// debugger:print a
// check:[...]$8 = 22
// debugger:print b
// check:[...]$9 = 23
// debugger:print c
// check:[...]$10 = 24.5
// debugger:continue

// FUNCTION CALL
// debugger:print x
// check:[...]$11 = 25
// debugger:print y
// check:[...]$12 = 26
// debugger:print z
// check:[...]$13 = 27.5
// debugger:continue

// EXPR
// debugger:print x
// check:[...]$14 = 28
// debugger:print y
// check:[...]$15 = 29
// debugger:print z
// check:[...]$16 = 30.5
// debugger:continue

// RETURN EXPR
// debugger:print x
// check:[...]$17 = 31
// debugger:print y
// check:[...]$18 = 32
// debugger:print z
// check:[...]$19 = 33.5
// debugger:continue

// ARITHMETIC EXPR
// debugger:print x
// check:[...]$20 = 34
// debugger:print y
// check:[...]$21 = 35
// debugger:print z
// check:[...]$22 = 36.5
// debugger:continue

// IF EXPR
// debugger:print x
// check:[...]$23 = 37
// debugger:print y
// check:[...]$24 = 38
// debugger:print z
// check:[...]$25 = 39.5
// debugger:continue

// WHILE EXPR
// debugger:print x
// check:[...]$26 = 40
// debugger:print y
// check:[...]$27 = 41
// debugger:print z
// check:[...]$28 = 42
// debugger:continue

// LOOP EXPR
// debugger:print x
// check:[...]$29 = 43
// debugger:print y
// check:[...]$30 = 44
// debugger:print z
// check:[...]$31 = 45
// debugger:continue

#[allow(unused_variable)];

fn immediate_args(a: int, b: bool, c: f64) {
    ()
}

struct BigStruct {
    a: u64,
    b: u64,
    c: u64,
    d: u64,
    e: u64,
    f: u64,
    g: u64,
    h: u64
}

fn non_immediate_args(a: BigStruct, b: BigStruct) {
    ()
}

fn binding(a: i64, b: u64, c: f64) {
    let x = 0;
}

fn assignment(mut a: u64, b: u64, c: f64) {
    a = b;
}

fn function_call(x: u64, y: u64, z: f64) {
    std::io::stdio::print("Hi!")
}

fn identifier(x: u64, y: u64, z: f64) -> u64 {
    x
}

fn return_expr(x: u64, y: u64, z: f64) -> u64 {
    return x;
}

fn arithmetic_expr(x: u64, y: u64, z: f64) -> u64 {
    x + y
}

fn if_expr(x: u64, y: u64, z: f64) -> u64 {
    if x + y < 1000 {
        x
    } else {
        y
    }
}

fn while_expr(mut x: u64, y: u64, z: u64) -> u64 {
    while x + y < 1000 {
        x += z
    }
    return x;
}

fn loop_expr(mut x: u64, y: u64, z: u64) -> u64 {
    loop {
        x += z;

        if x + y > 1000 {
            return x;
        }
    }
}

fn main() {
    immediate_args(1, true, 2.5);

    non_immediate_args(
        BigStruct {
            a: 3,
            b: 4,
            c: 5,
            d: 6,
            e: 7,
            f: 8,
            g: 9,
            h: 10
        },
        BigStruct {
            a: 11,
            b: 12,
            c: 13,
            d: 14,
            e: 15,
            f: 16,
            g: 17,
            h: 18
        }
    );

    binding(19, 20, 21.5);
    assignment(22, 23, 24.5);
    function_call(25, 26, 27.5);
    identifier(28, 29, 30.5);
    return_expr(31, 32, 33.5);
    arithmetic_expr(34, 35, 36.5);
    if_expr(37, 38, 39.5);
    while_expr(40, 41, 42);
    loop_expr(43, 44, 45);
}
