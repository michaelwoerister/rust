// Copyright 2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use mir::repr::*;
use serialize::{Decodable, Decoder, Encodable, Encoder};

// Decoding of mir::repr::Terminator is implemented manually because #[derive]
// can't handle fixed size arrays it seems. At some point it would make sense
// to change the data type definition, so it doesn't contain fixed size arrays.
// Then this manually implementation can be replaced by a generated one.

const TERMINATOR_GO_TO: u8      = 0;
const TERMINATOR_PANIC: u8      = 1;
const TERMINATOR_IF: u8         = 2;
const TERMINATOR_SWITCH: u8     = 3;
const TERMINATOR_SWITCH_INT: u8 = 4;
const TERMINATOR_DIVERGE: u8    = 5;
const TERMINATOR_RETURN: u8     = 6;
const TERMINATOR_CALL: u8       = 7;

impl<'tcx> Encodable for Terminator<'tcx> {
    fn encode<S: Encoder>(&self, s: &mut S) -> Result<(), S::Error> {
        match *self {
            Terminator::Goto { ref target } => {
                try! { s.emit_u8(TERMINATOR_GO_TO) };
                try! { Encodable::encode(target, s) };
            }
            Terminator::Panic { ref target } => {
                try! { s.emit_u8(TERMINATOR_PANIC) };
                try! { Encodable::encode(target, s) };
            }
            Terminator::If { ref cond, ref targets } => {
                try! { s.emit_u8(TERMINATOR_IF) };
                try! { Encodable::encode(cond, s) };
                try! { Encodable::encode(&targets[0], s) };
                try! { Encodable::encode(&targets[1], s) };
            }
            Terminator::Switch { ref discr, ref adt_def, ref targets } => {
                try! { s.emit_u8(TERMINATOR_SWITCH) };
                try! { Encodable::encode(discr, s) };
                try! { Encodable::encode(adt_def, s) };
                try! { Encodable::encode(targets, s) };
            }
            Terminator::SwitchInt { ref discr, ref switch_ty, ref values, ref targets } => {
                try! { s.emit_u8(TERMINATOR_SWITCH_INT) };
                try! { Encodable::encode(discr, s) };
                try! { Encodable::encode(switch_ty, s) };
                try! { Encodable::encode(values, s) };
                try! { Encodable::encode(targets, s) };
            }
            Terminator::Diverge => {
                try! { s.emit_u8(TERMINATOR_DIVERGE) };
            }
            Terminator::Return => {
                try! { s.emit_u8(TERMINATOR_RETURN) };
            }
            Terminator::Call { ref data, ref targets } => {
                try! { s.emit_u8(TERMINATOR_CALL) };
                try! { Encodable::encode(data, s) };
                try! { Encodable::encode(&targets[0], s) };
                try! { Encodable::encode(&targets[1], s) };
            }
        }
        Ok(())
    }
}


impl<'tcx> Decodable for Terminator<'tcx> {
    fn decode<S: Decoder>(s: &mut S) -> Result<Terminator<'tcx>, S::Error> {
        let discr = try! { s.read_u8() };

        let terminator = match discr {
            TERMINATOR_GO_TO => Terminator::Goto {
                target: try! { Decodable::decode(s) }
            },
            TERMINATOR_PANIC => Terminator::Panic {
                target: try! { Decodable::decode(s) }
            },
            TERMINATOR_IF => Terminator::If {
                cond: try! { Decodable::decode(s) },
                targets: [try! { Decodable::decode(s) },
                          try! { Decodable::decode(s) }],
            },
            TERMINATOR_SWITCH => Terminator::Switch {
                discr: try! { Decodable::decode(s) },
                adt_def: try! { Decodable::decode(s) },
                targets: try! { Decodable::decode(s) },
            },
            TERMINATOR_SWITCH_INT => Terminator::SwitchInt {
                discr: try! { Decodable::decode(s) },
                switch_ty: try! { Decodable::decode(s) },
                values: try! { Decodable::decode(s) },
                targets: try! { Decodable::decode(s) },
            },
            TERMINATOR_DIVERGE => Terminator::Diverge,
            TERMINATOR_RETURN => Terminator::Return,
            TERMINATOR_CALL => Terminator::Call {
                data: try! { Decodable::decode(s) },
                targets: [try! { Decodable::decode(s) },
                          try! { Decodable::decode(s) }],
            },
            _ => unreachable!()
        };
        Ok(terminator)
    }
}
