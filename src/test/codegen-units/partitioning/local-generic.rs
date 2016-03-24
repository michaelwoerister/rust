// Copyright 2016 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// ignore-tidy-linelength
// compile-flags:-Zprint-trans-items=eager

#![allow(dead_code)]
#![crate_type="lib"]

// Used in different modules/codegen units but always instantiated in the same
// codegen unit.

//~ TRANS_ITEM fn local_generic::generic[0]<u32> @@ local_generic-0g[E]
//~ TRANS_ITEM fn local_generic::generic[0]<u64> @@ local_generic-0g[E]
//~ TRANS_ITEM fn local_generic::generic[0]<char> @@ local_generic-0g[E]
//~ TRANS_ITEM fn local_generic::generic[0]<&str> @@ local_generic-0g[E]
pub fn generic<T>(x: T) -> T { x }

//~ TRANS_ITEM fn local_generic::user[0] @@ local_generic[E]
fn user() {
    let _ = generic(0u32);
}

mod mod1 {
    pub use super::generic;

    //~ TRANS_ITEM fn local_generic::mod1[0]::user[0] @@ local_generic-mod1[E]
    fn user() {
        let _ = generic(0u64);
    }

    mod mod1 {
        use super::generic;

        //~ TRANS_ITEM fn local_generic::mod1[0]::mod1[0]::user[0] @@ local_generic-mod1-mod1[E]
        fn user() {
            let _ = generic('c');
        }
    }
}

mod mod2 {
    use super::generic;

    //~ TRANS_ITEM fn local_generic::mod2[0]::user[0] @@ local_generic-mod2[E]
    fn user() {
        let _ = generic("abc");
    }
}

//~ TRANS_ITEM drop-glue i8 @@ __rustc_fallback_codegen_unit[E]
