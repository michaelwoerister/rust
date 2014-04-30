// Copyright 2013-2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// ignore-android: FIXME(#10381)

// compile-flags:-g

// === GDB TESTS ===================================================================================

// gdb-command:rbreak zzz
// gdb-command:run

// gdb-command:finish
// gdb-command:print a
// gdb-check:$1 = 1
// gdb-command:print b
// gdb-check:$2 = false
// gdb-command:continue

// gdb-command:finish
// gdb-command:print a
// gdb-check:$3 = 2
// gdb-command:print b
// gdb-check:$4 = 3
// gdb-command:print c
// gdb-check:$5 = 4
// gdb-command:continue

// gdb-command:finish
// gdb-command:print a
// gdb-check:$6 = 5
// gdb-command:print b
// gdb-check:$7 = {6, 7}
// gdb-command:continue

// gdb-command:finish
// gdb-command:print h
// gdb-check:$8 = 8
// gdb-command:print i
// gdb-check:$9 = {a = 9, b = 10}
// gdb-command:print j
// gdb-check:$10 = 11
// gdb-command:continue

// gdb-command:finish
// gdb-command:print k
// gdb-check:$11 = 12
// gdb-command:print l
// gdb-check:$12 = 13
// gdb-command:continue

// gdb-command:finish
// gdb-command:print m
// gdb-check:$13 = 14
// gdb-command:print n
// gdb-check:$14 = 16
// gdb-command:continue

// gdb-command:finish
// gdb-command:print o
// gdb-check:$15 = 18
// gdb-command:continue

// gdb-command:finish
// gdb-command:print p
// gdb-check:$16 = 19
// gdb-command:print q
// gdb-check:$17 = 20
// gdb-command:print r
// gdb-check:$18 = {a = 21, b = 22}
// gdb-command:continue

// gdb-command:finish
// gdb-command:print s
// gdb-check:$19 = 24
// gdb-command:print t
// gdb-check:$20 = 23
// gdb-command:continue

// gdb-command:finish
// gdb-command:print u
// gdb-check:$21 = 25
// gdb-command:print v
// gdb-check:$22 = 26
// gdb-command:print w
// gdb-check:$23 = 27
// gdb-command:print x
// gdb-check:$24 = 28
// gdb-command:print y
// gdb-check:$25 = 29
// gdb-command:print z
// gdb-check:$26 = 30
// gdb-command:print ae
// gdb-check:$27 = 31
// gdb-command:print oe
// gdb-check:$28 = 32
// gdb-command:print ue
// gdb-check:$29 = 33
// gdb-command:continue

// gdb-command:finish
// gdb-command:print aa
// gdb-check:$30 = {34, 35}
// gdb-command:continue

// gdb-command:finish
// gdb-command:print bb
// gdb-check:$31 = {36, 37}
// gdb-command:continue

// gdb-command:finish
// gdb-command:print cc
// gdb-check:$32 = 38
// gdb-command:continue

// gdb-command:finish
// gdb-command:print *ee
// gdb-check:$33 = {43, 44, 45}
// gdb-command:continue

// gdb-command:finish
// gdb-command:print *ff
// gdb-check:$34 = 46
// gdb-command:print gg
// gdb-check:$35 = {47, 48}
// gdb-command:continue

// gdb-command:finish
// gdb-command:print *hh
// gdb-check:$36 = 50
// gdb-command:continue

// gdb-command:finish
// gdb-command:print ii
// gdb-check:$37 = 51
// gdb-command:continue

// gdb-command:finish
// gdb-command:print *jj
// gdb-check:$38 = 52
// gdb-command:continue

// gdb-command:finish
// gdb-command:print kk
// gdb-check:$39 = 53
// gdb-command:print ll
// gdb-check:$40 = 54
// gdb-command:continue

// gdb-command:finish
// gdb-command:print mm
// gdb-check:$41 = 55
// gdb-command:print *nn
// gdb-check:$42 = 56
// gdb-command:continue

// gdb-command:finish
// gdb-command:print oo
// gdb-check:$43 = 57
// gdb-command:print pp
// gdb-check:$44 = 58
// gdb-command:print qq
// gdb-check:$45 = 59
// gdb-command:continue

// gdb-command:finish
// gdb-command:print rr
// gdb-check:$46 = 60
// gdb-command:print ss
// gdb-check:$47 = 61
// gdb-command:print tt
// gdb-check:$48 = 62
// gdb-command:continue


// === LLDB TESTS ==================================================================================
// lldb-command:run

// lldb-command:print a
// lldb-lldb-check:[...]$0 = 1
// lldb-command:print b
// lldb-lldb-check:[...]$1 = false
// lldb-command:continue

// lldb-command:print a
// lldb-lldb-check:[...]$2 = 2
// lldb-command:print b
// lldb-lldb-check:[...]$3 = 3
// lldb-command:print c
// lldb-lldb-check:[...]$4 = 4
// lldb-command:continue

// lldb-command:print a
// lldb-lldb-check:[...]$5 = 5
// lldb-command:print b
// lldb-lldb-check:[...]$6 = (6, 7)
// lldb-command:continue

// lldb-command:print h
// lldb-lldb-check:[...]$7 = 8
// lldb-command:print i
// lldb-lldb-check:[...]$8 = Struct { a: 9, b: 10 }
// lldb-command:print j
// lldb-lldb-check:[...]$9 = 11
// lldb-command:continue

// lldb-command:print k
// lldb-lldb-check:[...]$10 = 12
// lldb-command:print l
// lldb-lldb-check:[...]$11 = 13
// lldb-command:continue

// lldb-command:print m
// lldb-lldb-check:[...]$12 = 14
// lldb-command:print n
// lldb-lldb-check:[...]$13 = 16
// lldb-command:continue

// lldb-command:print o
// lldb-lldb-check:[...]$14 = 18
// lldb-command:continue

// lldb-command:print p
// lldb-lldb-check:[...]$15 = 19
// lldb-command:print q
// lldb-lldb-check:[...]$16 = 20
// lldb-command:print r
// lldb-lldb-check:[...]$17 = Struct { a: 21, b: 22 }
// lldb-command:continue

// lldb-command:print s
// lldb-lldb-check:[...]$18 = 24
// lldb-command:print t
// lldb-lldb-check:[...]$19 = 23
// lldb-command:continue

// lldb-command:print u
// lldb-lldb-check:[...]$20 = 25
// lldb-command:print v
// lldb-lldb-check:[...]$21 = 26
// lldb-command:print w
// lldb-lldb-check:[...]$22 = 27
// lldb-command:print x
// lldb-lldb-check:[...]$23 = 28
// lldb-command:print y
// lldb-lldb-check:[...]$24 = 29
// lldb-command:print z
// lldb-lldb-check:[...]$25 = 30
// lldb-command:print ae
// lldb-lldb-check:[...]$26 = 31
// lldb-command:print oe
// lldb-lldb-check:[...]$27 = 32
// lldb-command:print ue
// lldb-lldb-check:[...]$28 = 33
// lldb-command:continue

// lldb-command:print aa
// lldb-lldb-check:[...]$29 = (34, 35)
// lldb-command:continue

// lldb-command:print bb
// lldb-lldb-check:[...]$30 = (36, 37)
// lldb-command:continue

// lldb-command:print cc
// lldb-lldb-check:[...]$31 = 38
// lldb-command:continue

// lldb-command:print *ee
// lldb-lldb-check:[...]$32 = (43, 44, 45)
// lldb-command:continue

// lldb-command:print *ff
// lldb-lldb-check:[...]$33 = 46
// lldb-command:print gg
// lldb-lldb-check:[...]$34 = (47, 48)
// lldb-command:continue

// lldb-command:print *hh
// lldb-lldb-check:[...]$35 = 50
// lldb-command:continue

// lldb-command:print ii
// lldb-lldb-check:[...]$36 = 51
// lldb-command:continue

// lldb-command:print *jj
// lldb-lldb-check:[...]$37 = 52
// lldb-command:continue

// lldb-command:print kk
// lldb-lldb-check:[...]$38 = 53
// lldb-command:print ll
// lldb-lldb-check:[...]$39 = 54
// lldb-command:continue

// lldb-command:print mm
// lldb-lldb-check:[...]$40 = 55
// lldb-command:print *nn
// lldb-lldb-check:[...]$41 = 56
// lldb-command:continue

// lldb-command:print oo
// lldb-lldb-check:[...]$42 = 57
// lldb-command:print pp
// lldb-lldb-check:[...]$43 = 58
// lldb-command:print qq
// lldb-lldb-check:[...]$44 = 59
// lldb-command:continue

// lldb-command:print rr
// lldb-lldb-check:[...]$45 = 60
// lldb-command:print ss
// lldb-lldb-check:[...]$46 = 61
// lldb-command:print tt
// lldb-lldb-check:[...]$47 = 62
// lldb-command:continue

#![allow(unused_variable)]

struct Struct {
    a: i64,
    b: i32
}

enum Univariant {
    Unit(i32)
}

struct TupleStruct (f64, int);


fn simple_tuple((a, b): (int, bool)) {
    zzz(); // #break
}

fn nested_tuple((a, (b, c)): (int, (u16, u16))) {
    zzz(); // #break
}

fn destructure_only_first_level((a, b): (int, (u32, u32))) {
    zzz(); // #break
}

fn struct_as_tuple_element((h, i, j): (i16, Struct, i16)) {
    zzz(); // #break
}

fn struct_pattern(Struct { a: k, b: l }: Struct) {
    zzz(); // #break
}

fn ignored_tuple_element((m, _, n): (int, u16, i32)) {
    zzz(); // #break
}

fn ignored_struct_field(Struct { b: o, .. }: Struct) {
    zzz(); // #break
}

fn one_struct_destructured_one_not((Struct { a: p, b: q }, r): (Struct, Struct)) {
    zzz(); // #break
}

fn different_order_of_struct_fields(Struct { b: s, a: t }: Struct ) {
    zzz(); // #break
}

fn complex_nesting(((u,   v  ), ((w,   (x,   Struct { a: y, b: z})), Struct { a: ae, b: oe }), ue ):
                   ((i16, i32), ((i64, (i32, Struct,             )), Struct                 ), u16))
{
    zzz(); // #break
}

fn managed_box(&aa: &(int, int)) {
    zzz(); // #break
}

fn borrowed_pointer(&bb: &(int, int)) {
    zzz(); // #break
}

fn contained_borrowed_pointer((&cc, _): (&int, int)) {
    zzz(); // #break
}

fn ref_binding(ref ee: (int, int, int)) {
    zzz(); // #break
}

fn ref_binding_in_tuple((ref ff, gg): (int, (int, int))) {
    zzz(); // #break
}

fn ref_binding_in_struct(Struct { b: ref hh, .. }: Struct) {
    zzz(); // #break
}

fn univariant_enum(Unit(ii): Univariant) {
    zzz(); // #break
}

fn univariant_enum_with_ref_binding(Unit(ref jj): Univariant) {
    zzz(); // #break
}

fn tuple_struct(TupleStruct(kk, ll): TupleStruct) {
    zzz(); // #break
}

fn tuple_struct_with_ref_binding(TupleStruct(mm, ref nn): TupleStruct) {
    zzz(); // #break
}

fn multiple_arguments((oo, pp): (int, int), qq : int) {
    zzz(); // #break
}

fn main() {
    simple_tuple((1, false));
    nested_tuple((2, (3, 4)));
    destructure_only_first_level((5, (6, 7)));
    struct_as_tuple_element((8, Struct { a: 9, b: 10 }, 11));
    struct_pattern(Struct { a: 12, b: 13 });
    ignored_tuple_element((14, 15, 16));
    ignored_struct_field(Struct { a: 17, b: 18 });
    one_struct_destructured_one_not((Struct { a: 19, b: 20 }, Struct { a: 21, b: 22 }));
    different_order_of_struct_fields(Struct { a: 23, b: 24 });
    complex_nesting(((25, 26), ((27, (28, Struct { a: 29, b: 30})), Struct { a: 31, b: 32 }), 33));
    managed_box(&(34, 35));
    borrowed_pointer(&(36, 37));
    contained_borrowed_pointer((&38, 39));
    ref_binding((43, 44, 45));
    ref_binding_in_tuple((46, (47, 48)));
    ref_binding_in_struct(Struct { a: 49, b: 50 });
    univariant_enum(Unit(51));
    univariant_enum_with_ref_binding(Unit(52));
    tuple_struct(TupleStruct(53.0, 54));
    tuple_struct_with_ref_binding(TupleStruct(55.0, 56));
    multiple_arguments((57, 58), 59);

    fn nested_function(rr: int, (ss, tt): (int, int)) {
        zzz(); // #break
    }

    nested_function(60, (61, 62));
}

fn zzz() { () }
