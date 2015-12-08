// Copyright 2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use serialize::{Decodable, Decoder};
use hir::InlineAsm;
use std::mem;

// Sometimes we need to deserialize something that contains a reference to an
// InlineAsm value. Normally these values are owned by the HIR, but that is not
// available for items from external crates. We help ourselves by boxing the
// deserialized value and then letting it live forever, so we can safely return
// a readonly reference to it.
impl<'a> Decodable for &'a InlineAsm {
    fn decode<D: Decoder>(d: &mut D) -> Result<&'a InlineAsm, D::Error> {
        let inline_asm: InlineAsm = try! { Decodable::decode(d) };
        let boxed = Box::new(inline_asm);
        let ptr = Box::into_raw(boxed);
        unsafe {
            Ok(mem::transmute(ptr))
        }
    }
}
