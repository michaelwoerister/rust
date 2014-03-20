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

// debugger:print auto_one
// check:[...]$0 = One

// debugger:print auto_two
// check:[...]$1 = Two

// debugger:print auto_three
// check:[...]$2 = Three

// debugger:print manual_one_hundred
// check:[...]$3 = OneHundred

// debugger:print manual_one_thousand
// check:[...]$4 = OneThousand

// debugger:print manual_one_million
// check:[...]$5 = OneMillion

// debugger:print single_variant
// check:[...]$6 = TheOnlyVariant

#[allow(unused_variable)];

enum AutoDiscriminant {
    One,
    Two,
    Three
}

enum ManualDiscriminant {
    OneHundred = 100,
    OneThousand = 1000,
    OneMillion = 1000000
}

enum SingleVariant {
    TheOnlyVariant
}

fn main() {

    let auto_one = One;
    let auto_two = Two;
    let auto_three = Three;

    let manual_one_hundred = OneHundred;
    let manual_one_thousand = OneThousand;
    let manual_one_million = OneMillion;

    let single_variant = TheOnlyVariant;

    (); // #break
}
