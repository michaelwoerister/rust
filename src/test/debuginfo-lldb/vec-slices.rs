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

// debugger:print empty.length
// check:[...]$0 = 0

// debugger:print singleton.length
// check:[...]$1 = 1
// debugger:print *(int64_t*)(singleton.data_ptr)
// check:[...]$2 = 1

// debugger:print multiple.length
// check:[...]$3 = 4
// debugger:print *(((int64_t*)(multiple.data_ptr)) + 0)
// check:[...]$4 = 2
// debugger:print *(((int64_t*)(multiple.data_ptr)) + 1)
// check:[...]$5 = 3
// debugger:print *(((int64_t*)(multiple.data_ptr)) + 2)
// check:[...]$6 = 4
// debugger:print *(((int64_t*)(multiple.data_ptr)) + 3)
// check:[...]$7 = 5

// debugger:print slice_of_slice.length
// check:[...]$8 = 2
// debugger:print *(((int64_t*)(slice_of_slice.data_ptr)) + 0)
// check:[...]$9 = 3
// debugger:print *(((int64_t*)(slice_of_slice.data_ptr)) + 1)
// check:[...]$10 = 4

// debugger:print padded_tuple.length
// check:[...]$11 = 2
// debugger:print padded_tuple.data_ptr[0]
// check:[...]$12 = (6, 7)
// debugger:print padded_tuple.data_ptr[1]
// check:[...]$13 = (8, 9)

// debugger:print padded_struct.length
// check:[...]$14 = 2
// debugger:print padded_struct.data_ptr[0]
// check:[...]$15 = (x = 10, y = 11, z = 12)
// debugger:print padded_struct.data_ptr[1]
// check:[...]$16 = (x = 13, y = 14, z = 15)

#[allow(unused_variable)];

struct AStruct {
    x: i16,
    y: i32,
    z: i16
}

fn main() {
    let empty: &[i64] = &[];
    let singleton: &[i64] = &[1];
    let multiple: &[i64] = &[2, 3, 4, 5];
    let slice_of_slice = multiple.slice(1,3);

    let padded_tuple: &[(i32, i16)] = &[(6, 7), (8, 9)];

    let padded_struct: &[AStruct] = &[
        AStruct { x: 10, y: 11, z: 12 },
        AStruct { x: 13, y: 14, z: 15 }
    ];

    (); // #break
}
