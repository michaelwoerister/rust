// Copyright 2016 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.


// This test case tests the ICH implementation for structs.

// The general pattern followed here is: Change one thing between rev1 and rev2
// and make sure that the hash has changed, then change nothing between rev2 and
// rev3 and make sure that the hash has not changed.

// successful-compile-allowed
// revisions: cfail1 cfail2 cfail3
// compile-flags: -Z query-dep-graph


#![allow(warnings)]
#![feature(rustc_attrs)]
#![crate_type="rlib"]

// Empty struct ----------------------------------------------------------------
#[rustc_clean(label="Hir", cfg="cfail2")]
struct EmptyStruct;


// Visibility ------------------------------------------------------------------
#[cfg(cfail1)]
struct Visibility;

#[cfg(not(cfail1))]
#[rustc_dirty(label="Hir", cfg="cfail2")]
#[rustc_clean(label="Hir", cfg="cfail3")]
#[rustc_metadata_clean(cfg="cfail3")]
pub struct Visibility;


// Layout ----------------------------------------------------------------------
#[cfg(cfail1)]
pub struct LayoutPacked;

#[cfg(not(cfail1))]
#[rustc_dirty(label="Hir", cfg="cfail2")]
#[rustc_clean(label="Hir", cfg="cfail3")]
#[rustc_metadata_dirty(cfg="cfail2")]
#[rustc_metadata_clean(cfg="cfail3")]
#[repr(packed)]
pub struct LayoutPacked;

#[cfg(cfail1)]
struct LayoutC;

#[cfg(not(cfail1))]
#[rustc_dirty(label="Hir", cfg="cfail2")]
#[rustc_clean(label="Hir", cfg="cfail3")]
#[rustc_metadata_dirty(cfg="cfail2")]
#[rustc_metadata_clean(cfg="cfail3")]
#[repr(C)]
struct LayoutC;


// Tuple Struct Change Field Type ----------------------------------------------

#[cfg(cfail1)]
struct TupleStructFieldType(i32);

#[cfg(not(cfail1))]
#[rustc_dirty(label="Hir", cfg="cfail2")]
#[rustc_clean(label="Hir", cfg="cfail3")]
#[rustc_metadata_dirty(cfg="cfail2")]
#[rustc_metadata_clean(cfg="cfail3")]
struct TupleStructFieldType(u32);


// Tuple Struct Add Field ------------------------------------------------------

#[cfg(cfail1)]
struct TupleStructAddField(i32);

#[cfg(not(cfail1))]
#[rustc_dirty(label="Hir", cfg="cfail2")]
#[rustc_clean(label="Hir", cfg="cfail3")]
#[rustc_metadata_dirty(cfg="cfail2")]
#[rustc_metadata_clean(cfg="cfail3")]
struct TupleStructAddField(i32, u32);


// Tuple Struct Field Visibility -----------------------------------------------

#[cfg(cfail1)]
struct TupleStructFieldVisibility(char);

#[cfg(not(cfail1))]
#[rustc_dirty(label="Hir", cfg="cfail2")]
#[rustc_clean(label="Hir", cfg="cfail3")]
#[rustc_metadata_dirty(cfg="cfail2")]
#[rustc_metadata_clean(cfg="cfail3")]
struct TupleStructFieldVisibility(pub char);


// Record Struct Field Type ----------------------------------------------------

#[cfg(cfail1)]
struct RecordStructFieldType { x: f32 }

#[cfg(not(cfail1))]
#[rustc_dirty(label="Hir", cfg="cfail2")]
#[rustc_clean(label="Hir", cfg="cfail3")]
#[rustc_metadata_dirty(cfg="cfail2")]
#[rustc_metadata_clean(cfg="cfail3")]
struct RecordStructFieldType { x: u64 }


// Record Struct Field Name ----------------------------------------------------

#[cfg(cfail1)]
struct RecordStructFieldName { x: f32 }

#[cfg(not(cfail1))]
#[rustc_dirty(label="Hir", cfg="cfail2")]
#[rustc_clean(label="Hir", cfg="cfail3")]
#[rustc_metadata_dirty(cfg="cfail2")]
#[rustc_metadata_clean(cfg="cfail3")]
struct RecordStructFieldName { y: f32 }


// Record Struct Add Field -----------------------------------------------------

#[cfg(cfail1)]
struct RecordStructAddField { x: f32 }

#[cfg(not(cfail1))]
#[rustc_dirty(label="Hir", cfg="cfail2")]
#[rustc_clean(label="Hir", cfg="cfail3")]
#[rustc_metadata_dirty(cfg="cfail2")]
#[rustc_metadata_clean(cfg="cfail3")]
struct RecordStructAddField { x: f32, y: () }


// Record Struct Field Visibility ----------------------------------------------

#[cfg(cfail1)]
struct RecordStructFieldVisibility { x: f32 }

#[cfg(not(cfail1))]
#[rustc_dirty(label="Hir", cfg="cfail2")]
#[rustc_clean(label="Hir", cfg="cfail3")]
#[rustc_metadata_dirty(cfg="cfail2")]
#[rustc_metadata_clean(cfg="cfail3")]
struct RecordStructFieldVisibility { pub x: f32 }


// Add Lifetime Parameter ------------------------------------------------------

#[cfg(cfail1)]
struct AddLifetimeParameter<'a>(&'a f32, &'a f64);

#[cfg(not(cfail1))]
#[rustc_dirty(label="Hir", cfg="cfail2")]
#[rustc_clean(label="Hir", cfg="cfail3")]
#[rustc_metadata_dirty(cfg="cfail2")]
#[rustc_metadata_clean(cfg="cfail3")]
struct AddLifetimeParameter<'a, 'b>(&'a f32, &'b f64);


// Add Lifetime Parameter Bound ------------------------------------------------

#[cfg(cfail1)]
struct AddLifetimeParameterBound<'a, 'b>(&'a f32, &'b f64);

#[cfg(not(cfail1))]
#[rustc_dirty(label="Hir", cfg="cfail2")]
#[rustc_clean(label="Hir", cfg="cfail3")]
#[rustc_metadata_dirty(cfg="cfail2")]
#[rustc_metadata_clean(cfg="cfail3")]
struct AddLifetimeParameterBound<'a, 'b: 'a>(&'a f32, &'b f64);

#[cfg(cfail1)]
struct AddLifetimeParameterBoundWhereClause<'a, 'b>(&'a f32, &'b f64);

#[cfg(not(cfail1))]
#[rustc_dirty(label="Hir", cfg="cfail2")]
#[rustc_clean(label="Hir", cfg="cfail3")]
#[rustc_metadata_dirty(cfg="cfail2")]
#[rustc_metadata_clean(cfg="cfail3")]
struct AddLifetimeParameterBoundWhereClause<'a, 'b>(&'a f32, &'b f64)
    where 'b: 'a;


// Add Type Parameter ----------------------------------------------------------

#[cfg(cfail1)]
struct AddTypeParameter<T1>(T1, T1);

#[cfg(not(cfail1))]
#[rustc_dirty(label="Hir", cfg="cfail2")]
#[rustc_clean(label="Hir", cfg="cfail3")]
#[rustc_metadata_dirty(cfg="cfail2")]
#[rustc_metadata_clean(cfg="cfail3")]
struct AddTypeParameter<T1, T2>(T1, T2);


// Add Type Parameter Bound ----------------------------------------------------

#[cfg(cfail1)]
struct AddTypeParameterBound<T>(T);

#[cfg(not(cfail1))]
#[rustc_dirty(label="Hir", cfg="cfail2")]
#[rustc_clean(label="Hir", cfg="cfail3")]
#[rustc_metadata_dirty(cfg="cfail2")]
#[rustc_metadata_clean(cfg="cfail3")]
struct AddTypeParameterBound<T: Send>(T);


#[cfg(cfail1)]
struct AddTypeParameterBoundWhereClause<T>(T);

#[cfg(not(cfail1))]
#[rustc_dirty(label="Hir", cfg="cfail2")]
#[rustc_clean(label="Hir", cfg="cfail3")]
#[rustc_metadata_dirty(cfg="cfail2")]
#[rustc_metadata_clean(cfg="cfail3")]
struct AddTypeParameterBoundWhereClause<T>(T) where T: Sync;

