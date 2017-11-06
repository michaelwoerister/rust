// Copyright 2012-2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use std::cmp;

#[inline]
fn write_to_vec(vec: &mut Vec<u8>, position: usize, byte: u8) {
    if position == vec.len() {
        vec.push(byte);
    } else {
        vec[position] = byte;
    }
}

#[inline]
fn write_slice_to_vec(output: &mut Vec<u8>, start_position: usize, input: &[u8]) {
    let input_len = input.len();
    let capacity = output.len() - start_position;
    let first_half = cmp::min(capacity, input_len);

    if first_half > 0 {
        (&mut output[start_position..]).copy_from_slice(&input[.. first_half]);
    }

    if first_half < input_len {
        output.extend_from_slice(&input[first_half..]);
    }
}

#[cfg(target_pointer_width = "32")]
const USIZE_LEB128_SIZE: usize = 5;
#[cfg(target_pointer_width = "64")]
const USIZE_LEB128_SIZE: usize = 10;

macro_rules! leb128_size {
    (u16) => (3);
    (u32) => (5);
    (u64) => (10);
    (u128) => (19);
    (usize) => (USIZE_LEB128_SIZE);
}

macro_rules! impl_write_unsigned_leb128 {
    ($fn_name:ident, $int_ty:ident) => (
        #[inline]
        pub fn $fn_name(out: &mut Vec<u8>, start_position: usize, mut value: $int_ty) -> usize {
            let mut encoded = [0u8; leb128_size!($int_ty)];

            for i in 0 .. leb128_size!($int_ty) {
                encoded[i] = (value as u8) & 0b0111_1111;
                value = value >> 7;

                if value == 0 {
                    let bytes_written = i + 1;
                    write_slice_to_vec(out, start_position, &encoded[0 .. bytes_written]);
                    return bytes_written
                } else {
                    encoded[i] |= 0b1000_0000;
                }
            }

            unreachable!()
        }
    )
}

impl_write_unsigned_leb128!(write_u16_leb128, u16);
impl_write_unsigned_leb128!(write_u32_leb128, u32);
impl_write_unsigned_leb128!(write_u64_leb128, u64);
impl_write_unsigned_leb128!(write_u128_leb128, u128);
impl_write_unsigned_leb128!(write_usize_leb128, usize);

macro_rules! impl_read_unsigned_leb128 {
    ($fn_name:ident, $int_ty:ident) => (
        #[inline]
        pub fn $fn_name(data: &[u8], start_position: usize) -> ($int_ty, usize) {
            // Copy things into a fixed size buffer so we can skip the bounds check
            let mut bytes = [0u8; leb128_size!($int_ty)];
            let copy_len = cmp::min(data.len() - start_position, leb128_size!($int_ty));
            (&mut bytes[0 .. copy_len])
                .copy_from_slice(&data[start_position .. start_position + copy_len]);

            let mut result = 0;

            for i in 0 .. leb128_size!($int_ty) {
                let byte = bytes[i];
                result |= ((byte & 0b0111_1111) as $int_ty) << i * 7;

                if (byte & 0b1000_0000) == 0 {
                    return (result, i + 1);
                }
            }

            unreachable!()
        }
    )
}

impl_read_unsigned_leb128!(read_u16_leb128, u16);
impl_read_unsigned_leb128!(read_u32_leb128, u32);
impl_read_unsigned_leb128!(read_u64_leb128, u64);
impl_read_unsigned_leb128!(read_u128_leb128, u128);
impl_read_unsigned_leb128!(read_usize_leb128, usize);

#[inline]
/// encodes an integer using signed leb128 encoding and stores
/// the result using a callback function.
///
/// The callback `write` is called once for each position
/// that is to be written to with the byte to be encoded
/// at that position.
pub fn write_signed_leb128_to<W>(mut value: i128, mut write: W) -> usize
    where W: FnMut(usize, u8)
{
    let mut position = 0;

    loop {
        let mut byte = (value as u8) & 0x7f;
        value >>= 7;
        let more = !((((value == 0) && ((byte & 0x40) == 0)) ||
                      ((value == -1) && ((byte & 0x40) != 0))));

        if more {
            byte |= 0x80; // Mark this byte to show that more bytes will follow.
        }

        write(position, byte);
        position += 1;

        if !more {
            break;
        }
    }
    position
}

pub fn write_signed_leb128(out: &mut Vec<u8>, start_position: usize, value: i128) -> usize {
    write_signed_leb128_to(value, |i, v| write_to_vec(out, start_position+i, v))
}

#[inline]
pub fn read_signed_leb128(data: &[u8], start_position: usize) -> (i128, usize) {
    let mut result = 0;
    let mut shift = 0;
    let mut position = start_position;
    let mut byte;

    loop {
        byte = data[position];
        position += 1;
        result |= ((byte & 0x7F) as i128) << shift;
        shift += 7;

        if (byte & 0x80) == 0 {
            break;
        }
    }

    if (shift < 64) && ((byte & 0x40) != 0) {
        // sign extend
        result |= -(1 << shift);
    }

    (result, position - start_position)
}

macro_rules! impl_test_unsigned_leb128 {
    ($test_name:ident, $write_fn_name:ident, $read_fn_name:ident, $int_ty:ident) => (
        #[test]
        fn $test_name() {
            let mut stream = Vec::new();

            for x in 0..62 {
                let pos = stream.len();
                let bytes_written = $write_fn_name(&mut stream, pos, (3u64 << x) as $int_ty);
                assert_eq!(stream.len(), pos + bytes_written);
            }

            let mut position = 0;
            for x in 0..62 {
                let expected = (3u64 << x) as $int_ty;
                let (actual, bytes_read) = $read_fn_name(&stream, position);
                assert_eq!(expected, actual);
                position += bytes_read;
            }
            assert_eq!(stream.len(), position);
        }
    )
}

impl_test_unsigned_leb128!(test_u16_leb128, write_u16_leb128, read_u16_leb128, u16);
impl_test_unsigned_leb128!(test_u32_leb128, write_u32_leb128, read_u32_leb128, u32);
impl_test_unsigned_leb128!(test_u64_leb128, write_u64_leb128, read_u64_leb128, u64);
impl_test_unsigned_leb128!(test_u128_leb128, write_u128_leb128, read_u128_leb128, u128);
impl_test_unsigned_leb128!(test_usize_leb128, write_usize_leb128, read_usize_leb128, usize);

#[test]
fn test_signed_leb128() {
    let values: Vec<_> = (-500..500).map(|i| i * 0x12345789ABCDEF).collect();
    let mut stream = Vec::new();
    for &x in &values {
        let pos = stream.len();
        let bytes_written = write_signed_leb128(&mut stream, pos, x);
        assert_eq!(stream.len(), pos + bytes_written);
    }
    let mut pos = 0;
    for &x in &values {
        let (value, bytes_read) = read_signed_leb128(&mut stream, pos);
        pos += bytes_read;
        assert_eq!(x, value);
    }
    assert_eq!(pos, stream.len());
}
