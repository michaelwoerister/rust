// Copyright 2012-2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// ignore-tidy-linelength
// compile-flags:-Zprint-codegen-items=eager

#![deny(dead_code)]

fn foo() {
    {
        fn foo() {}
        foo();
    }

    {
        fn foo() {}
        foo();
    }
}

fn bar() {
    fn baz() {}
    baz();
}

struct Struct { _x: i32 }

impl Struct {
    fn foo() {
		{
            fn foo() {}
	        foo();
	    }

	    {
            fn foo() {}
	        foo();
	    }
	}

    fn bar(&self) {
        {
            fn foo() {}
            foo();
        }

        {
            fn foo() {}
            foo();
        }
    }
}

fn main() {
    foo();
    bar();
    Struct::foo();
    let x = Struct { _x: 0 };
    x.bar();
}

//~ CODEGEN_ITEM fn non_generic_functions::foo[0]
//~ CODEGEN_ITEM fn non_generic_functions::foo[0]::foo[0]
//~ CODEGEN_ITEM fn non_generic_functions::foo[0]::foo[1]

//~ CODEGEN_ITEM fn non_generic_functions::bar[0]
//~ CODEGEN_ITEM fn non_generic_functions::bar[0]::baz[0]

//~ CODEGEN_ITEM fn non_generic_functions::Struct[0]::foo[0]
//~ CODEGEN_ITEM fn non_generic_functions::Struct[0]::foo[0]::foo[0]
//~ CODEGEN_ITEM fn non_generic_functions::Struct[0]::foo[0]::foo[1]

//~ CODEGEN_ITEM fn non_generic_functions::Struct[0]::bar[0]
//~ CODEGEN_ITEM fn non_generic_functions::Struct[0]::bar[0]::foo[0]
//~ CODEGEN_ITEM fn non_generic_functions::Struct[0]::bar[0]::foo[1]

//~ CODEGEN_ITEM fn non_generic_functions::main[0]

//~ CODEGEN_ITEM drop-glue i8
