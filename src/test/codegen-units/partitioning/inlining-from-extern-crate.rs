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
// compile-flags:-Zprint-trans-items=lazy

#![crate_type="lib"]

// aux-build:cgu_explicit_inlining.rs
extern crate cgu_explicit_inlining;

// This test makes sure that items inlined from external crates are privately
// instantiated in every codegen unit they are used in.

//~ TRANS_ITEM fn cgu_explicit_inlining::inlined[0] @@ inlining_from_extern_crate[P] inlining_from_extern_crate-mod1[P]
//~ TRANS_ITEM fn cgu_explicit_inlining::always_inlined[0] @@ inlining_from_extern_crate[P] inlining_from_extern_crate-mod2[P]

//~ TRANS_ITEM fn inlining_from_extern_crate::user[0] @@ inlining_from_extern_crate[E]
pub fn user()
{
    cgu_explicit_inlining::inlined();
    cgu_explicit_inlining::always_inlined();

    // does not generate a translation item in this crate
    cgu_explicit_inlining::never_inlined();
}

mod mod1 {
    use cgu_explicit_inlining;

    //~ TRANS_ITEM fn inlining_from_extern_crate::mod1[0]::user[0] @@ inlining_from_extern_crate-mod1[E]
    pub fn user()
    {
        cgu_explicit_inlining::inlined();

        // does not generate a translation item in this crate
        cgu_explicit_inlining::never_inlined();
    }
}

mod mod2 {
    use cgu_explicit_inlining;

    //~ TRANS_ITEM fn inlining_from_extern_crate::mod2[0]::user[0] @@ inlining_from_extern_crate-mod2[E]
    pub fn user()
    {
        cgu_explicit_inlining::always_inlined();

        // does not generate a translation item in this crate
        cgu_explicit_inlining::never_inlined();
    }
}

//~ TRANS_ITEM drop-glue i8 @@ __rustc_fallback_codegen_unit[E]
