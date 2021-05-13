// compile-flags:-g

// === GDB TESTS ===================================================================================

// Top-level function
// gdb-command:info functions -q function_names::main
// gdb-check:[...]static fn function_names::main();

// Implementations
// gdb-command:info functions -q function_names::.*::impl_function.*
// gdb-check:[...]static fn function_names::GenericStruct<T1,T2>::impl_function();
// gdb-check:[...]static fn function_names::Mod1::TestStruct2::impl_function();
// gdb-check:[...]static fn function_names::TestStruct1::impl_function();

// Trait implementations
// gdb-command:info functions -q function_names::.*::trait_function.*
// gdb-check:[...]static fn <function_names::GenericStruct<T,i32> as function_names::TestTrait1>::trait_function();
// gdb-check:[...]static fn <function_names::GenericStruct<[T; N],f32> as function_names::TestTrait1>::trait_function();
// gdb-check:[...]static fn <function_names::Mod1::TestStruct2 as function_names::Mod1::TestTrait2>::trait_function();
// gdb-check:[...]static fn <function_names::TestStruct1 as function_names::TestTrait1>::trait_function();

// Closure
// gdb-command:info functions -q function_names::.*::{{closure.*
// gdb-check:[...]static fn function_names::main::{{closure}}(*mut function_names::main::{closure#0});

// Generator
// Generators don't seem to appear in GDB's symbol table.

// === CDB TESTS ===================================================================================

// Top-level function
// cdb-command:x a!function_names::main
// cdb-check:[...] a!function_names::main (void)

// Implementations
// cdb-command:x a!function_names::*::impl_function*
// cdb-check:[...] a!function_names::Mod1::TestStruct2::impl_function (void)
// cdb-check:[...] a!function_names::TestStruct1::impl_function (void)
// cdb-check:[...] a!function_names::GenericStruct<i32, i32>::impl_function<i32, i32> (void)

// Trait implementations
// cdb-command:x a!function_names::*::trait_function*
// cdb-check:[...] a!function_names::__impl<function_names::TestTrait1 for GenericStruct<T, i32> >::trait_function<i32> (void)
// cdb-check:[...] a!function_names::__impl<function_names::TestTrait1 for GenericStruct<array<T,N>, f32> >::trait_function<i32, 0x1> (void)
// cdb-check:[...] a!function_names::Mod1::__impl<function_names::Mod1::TestTrait2 for TestStruct2>::trait_function (void)
// cdb-check:[...] a!function_names::__impl<function_names::TestTrait1 for TestStruct1>::trait_function (void)

// Closure
// cdb-command:x a!function_names::*::__closure*
// cdb-check:[...] a!function_names::main::__closure$0 (void)

// Generator
// cdb-command:x a!function_names::*::__generator*
// cdb-check:[...] a!function_names::main::__generator$1 (void)

#![allow(unused_variables)]
#![feature(omit_gdb_pretty_printer_section)]
#![omit_gdb_pretty_printer_section]
#![feature(generators, generator_trait)]

use Mod1::TestTrait2;
use std::ops::Generator;
use std::pin::Pin;

fn main() {
    // Implementations
    TestStruct1::impl_function();
    Mod1::TestStruct2::impl_function();
    GenericStruct::<i32, i32>::impl_function();

    // Trait implementations
    TestStruct1::trait_function();
    Mod1::TestStruct2::trait_function();
    GenericStruct::<i32, i32>::trait_function();
    GenericStruct::<[i32; 1], f32>::trait_function();

    // Closure
    let closure = || { TestStruct1 };
    closure();

    // Generator
    let mut generator = || { yield; return; };
    Pin::new(&mut generator).resume(());
}

struct TestStruct1;
trait TestTrait1 {
    fn trait_function();
}

// Implementation
impl TestStruct1 {
    pub fn impl_function() {}
}

// Implementation for a trait
impl TestTrait1 for TestStruct1 {
    fn trait_function() {}
}

// Implementation and implementation within a mod
mod Mod1 {
    pub struct TestStruct2;
    pub trait TestTrait2 {
        fn trait_function();
    }

    impl TestStruct2 {
        pub fn impl_function() {}
    }

    impl TestTrait2 for TestStruct2 {
        fn trait_function() {}
    }
}

struct GenericStruct<T1, T2>(std::marker::PhantomData<(T1, T2)>);

// Generic implementation
impl<T1, T2> GenericStruct<T1, T2> {
    pub fn impl_function() {}
}

// Generic trait implementation
impl<T> TestTrait1 for GenericStruct<T, i32> {
    fn trait_function() {}
}

// Generic trait implementation with const generics
impl<T, const N: usize> TestTrait1 for GenericStruct<[T; N], f32> {
    fn trait_function() {}
}
