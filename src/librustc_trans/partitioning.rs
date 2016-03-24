// Copyright 2016 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Partitioning Codegen Units for Incremental Compilation
//! ======================================================
//!
//! The most important opportunity for saving on compilation time with
//! incremental compilation is to avoid re-translating and re-optimizing code.
//! Since the unit of translation and optimization for LLVM is "modules" or, how
//! we call them "codegen units", the particulars of how much time can be saved
//! by incremental compilation are tightly linked to how the output program is
//! partitioned into these codegen units prior to passing it to LLVM -- especially because
//! we have to treat codegen units as opaque entities once they are created: There
//! is no way for us to incrementally update an existing LLVM module and so we have to
//! build any such module from scratch if it was affected by some change in the
//! source code.
//!
//! From that point of view it would make sense to maximize the number of codegen
//! units by, for example, putting each function into its own module. That way only
//! those modules would have to be re-compiled that were actually affected by
//! some change, minimizing collateral damage of functions being hit that could
//! be re-used but just happened to be located in a module that is re-compiled.
//!
//! However, since LLVM optimization does not work across module boundaries, using
//! such a highly granular partitioning would lead to very slow runtime code since
//! it would effectively prohibit inlining and other inter-procedure optimizations.
//! We want to avoid that as much as possible.
//!
//! Thus we end up with a trade off: The bigger the codegen units, the better
//! LLVM's optimizer can do its work, but also the smaller the compilation time
//! reduction we get from incremental compilation.
//!
//! Ideally, we would create a partitioning such that there are few big codegen
//! units with few interdependencies between them. For now though, we use the
//! following heuristic to determine the partitioning:
//!
//! - There are two codegen units for every source-level module:
//! - One for "stable", that is non-generic, code
//! - One for more "volatile" code, i.e. monomorphized instances of functions
//! defined in that module
//! - Code for monomorphized instances of functions from external crates gets
//! placed into every codegen unit that uses that instance.
//!
//! In order to see why this heuristic makes sense, let's take a look at when a
//! codegen unit can get invalidated:
//!
//! 1. The most straightforward case is when the BODY of a function or global
//! changes. Then the codegen unit containing the code for that item has to be
//! re-compiled. Note that this includes all codegen units containing monomorphized
//! instances of that
//! function and also all codegen units where the function has been inlined.
//!
//! 2. The next case is when the SIGNATURE of a function or global changes. In this
//! case, all codegen units containing a REFERENCE to that item have to be
//! re-compiled. This is a superset of case 1.
//!
//! 3. The final and most subtle case is when a REFERENCE to a generic function is
//! added or removed somewhere. Even though the definition of the function
//! might be unchanged, a new REFERENCE might introduce a new monomorphized
//! instance of this function which has to be placed and compiled somewhere.
//! Conversely, when removing a REFERENCE, it might have been the last one with
//! that particular set of generic arguments and thus we have to remove it.
//!
//! From the above we see that just using one codegen unit per source-level module
//! is not such a good idea, since just adding a REFERENCE to some generic item
//! somewhere else would invalidate everything within the module containing the
//! generic item. The heuristic above reduces this detrimental side-effect of
//! references a little by at least not touching the non-generic code of the
//! module.
//!
//! Monomorphized functions from external crates get some special handling. Since
//! we assume that the definition of such a function never changes, we can just
//! instantiate such a function in every codegen unit where it is referenced --
//! without having to fear that doing this will cause a lot of unnecessary
//! re-compilations. If such a reference is added or removed, the codegen unit
//! has to be re-translated anyway. (Note that this only makes sense if external
//! crates actually don't change frequently. For certain multi-crate projects
//! this might not be a valid assumption).

use collector::{InliningMap, TransItem};
use context::CrateContext;
use rustc::front::map as hir_map;
use rustc::middle::def_id::DefId;
use rustc::ty::{self, TyCtxt};
use std::collections::hash_map::Entry;
use std::cmp::{PartialOrd, Ordering};
use syntax::parse::token::{self, InternedString};
use util::nodemap::{FnvHashMap, FnvHashSet};

#[derive(Clone, Copy, Eq, PartialEq, Hash, Debug)]
pub enum InstantiationKind {
    Exported,
    Private,
    AvailableExternally,
}

impl PartialOrd<InstantiationKind> for InstantiationKind {
    fn partial_cmp(&self, other: &InstantiationKind) -> Option<Ordering> {
        if *self == *other {
            return Some(Ordering::Equal);
        }

        match (*self, *other) {
            (InstantiationKind::Exported,
             InstantiationKind::AvailableExternally) |
            (InstantiationKind::Exported, InstantiationKind::Private) |
            (InstantiationKind::Private,
             InstantiationKind::AvailableExternally) => Some(Ordering::Greater),
            _ => Some(Ordering::Less),
        }
    }
}

pub struct CodegenUnit<'tcx> {
    pub name: InternedString,
    pub items: FnvHashMap<TransItem<'tcx>, InstantiationKind>,
}

// Anything we can't find a proper codegen unit for goes into this.
const FALLBACK_CODEGEN_UNIT: &'static str = "__rustc_fallback_codegen_unit";

pub fn partition<'a, 'tcx, I>(ccx: &CrateContext<'a, 'tcx>,
                              trans_items: I,
                              inlining_map: InliningMap<'tcx>)
                              -> FnvHashMap<InternedString, CodegenUnit<'tcx>>
    where I: Iterator<Item = TransItem<'tcx>>
{
    let mut partitioner = Partitioner {
        ccx: ccx,
        codegen_units: FnvHashMap(),
        inlining_map: inlining_map,
    };

    for trans_item in trans_items {
        partitioner.place_translation_item(trans_item);
    }

    partitioner.codegen_units
}

struct Partitioner<'ccx, 'tcx> where 'tcx: 'ccx {
    ccx: &'ccx CrateContext<'ccx, 'tcx>,
    codegen_units: FnvHashMap<InternedString, CodegenUnit<'tcx>>,
    inlining_map: InliningMap<'tcx>,
}

impl<'ccx, 'tcx> Partitioner<'ccx, 'tcx> {
    fn place_translation_item(&mut self, trans_item: TransItem<'tcx>) {

        if !trans_item.is_from_extern_crate() {
            // For local items we always emit an exported symbol in its 'home'
            // codegen unit.
            self.add_to_codegen_unit(trans_item, InstantiationKind::Exported, trans_item);

            if trans_item.requests_inline(self.ccx.tcx()) {
                assert!(is_fn(trans_item));

                // If the item is going to be inlined, we also need to emit the
                // item in every codegen unit, where it is used.
                for usage in self.transitive_local_usages(trans_item) {
                    self.add_to_codegen_unit(trans_item,
                                             InstantiationKind::AvailableExternally,
                                             usage);
                }
            }
        } else {
            assert!(is_fn(trans_item));

            // Items from other crates are always emitted in every codegen unit
            // they are used in.
            for usage in self.transitive_local_usages(trans_item) {
                assert!(!usage.is_from_extern_crate());
                self.add_to_codegen_unit(trans_item, InstantiationKind::Private, usage);
            }
        }
    }

    fn add_to_codegen_unit(&mut self,
                           trans_item: TransItem<'tcx>,
                           instantiation_kind: InstantiationKind,
                           // The translation item that is used to compute
                           // the target codegen unit
                           cgu_trans_item: TransItem<'tcx>) {
        assert!(!cgu_trans_item.is_from_extern_crate());

        let codegen_unit_name = match self.characteristic_def_id_of_trans_item(cgu_trans_item) {
            Some(def_id) => {
                self.compute_codegen_unit_name(def_id,
                                               cgu_trans_item.is_lazily_instantiated())
            }
            None => InternedString::new(FALLBACK_CODEGEN_UNIT),
        };

        let mut codegen_unit = self.codegen_units
                                   .entry(codegen_unit_name.clone())
                                   .or_insert(CodegenUnit {
                                       name: codegen_unit_name,
                                       items: FnvHashMap(),
                                   });

        match codegen_unit.items.entry(trans_item) {
            Entry::Vacant(e) => {
                e.insert(instantiation_kind);
            }
            Entry::Occupied(mut e) => {
                if instantiation_kind > *e.get() {
                    *e.get_mut() = instantiation_kind;
                }
            }
        }
    }

    fn compute_codegen_unit_name(&mut self,
                                 def_id: DefId,
                                 lazy: bool)
                                 -> InternedString {
        // Unfortunately, TyCtxt::with_path() is a bit too clever for us here,
        // because will make a difference between directly and indirectly
        // referenced crates. We don't want that.
        let mut mod_path = String::with_capacity(64);

        if let Some(id) = self.ccx.tcx().map.as_local_node_id(def_id) {
            // Local paths don't contain the crate name by themselves, so we
            // prepend it.
            mod_path.push_str(&self.ccx.link_meta().crate_name[..]);
            mod_path.push_str("-");
            self.ccx
                .tcx()
                .map
                .with_path(id, |p| push_up_to_first_non_mod(p, &mut mod_path));
        } else {
            push_up_to_first_non_mod(self.ccx
                                         .sess()
                                         .cstore
                                         .extern_item_path(def_id)
                                         .iter()
                                         .cloned(),
                                     &mut mod_path);
        }

        if lazy {
            // Append something that cannot be a regular module name
            mod_path.push_str("0g");
        } else {
            // Remove that last '-' separator
            mod_path.pop();
        }

        return token::intern_and_get_ident(&mod_path[..]);

        // We only consider the path up to first non-module item.
        fn push_up_to_first_non_mod<PI>(path: PI, output: &mut String)
            where PI: Iterator<Item=hir_map::PathElem>
        {
            for path_elem in path {
                match path_elem {
                    hir_map::PathMod(name) => {
                        output.push_str(&name.as_str());
                        output.push_str("-");
                    }
                    hir_map::PathName(_) => break,
                }
            }
        }
    }

    fn characteristic_def_id_of_trans_item(&self,
                                           trans_item: TransItem<'tcx>)
                                           -> Option<DefId> {
        match trans_item {
            TransItem::Fn(instance) => {
                // For now, just use the actual DefId of the item, with the
                // consequence that, for example, methods are emitted in the
                // module where the impl is located instead of the one where
                // the self-type is located.
                Some(instance.def)
            }
            TransItem::DropGlue(t) => {
                characteristic_def_id_of_type(t)
            }
            TransItem::Static(node_id) => Some(self.ccx.tcx().map.local_def_id(node_id)),
        }
    }

    // Finds all items that need to have the full definition of `trans_item`
    // available, either because `trans_item` gets inlined or `trans_item` is
    // from an extern crate and we want to emit it in every user's codegen unit
    // as an optimization.
    // Note that this function, as its name says, needs to consider transitive
    // references too: If item A is inlined into item B and item B is inlined
    // into item C, we need both A and B to be available, when translating C,
    // although C only references A indirectly.
    fn transitive_local_usages(&self,
                               trans_item: TransItem<'tcx>)
                               -> FnvHashSet<TransItem<'tcx>> {
        let mut local_usages = FnvHashSet();
        let mut visited = FnvHashSet();

        // If this turns out to be redoing a lot of work for inlined functions,
        // we could do some caching of transitive uses.
        collect_transitive_local_usages(trans_item,
                                        self.ccx.tcx(),
                                        &self.inlining_map,
                                        &mut local_usages,
                                        &mut visited);
        return local_usages;

        fn collect_transitive_local_usages<'tcx>(trans_item: TransItem<'tcx>,
                                                 tcx: &TyCtxt<'tcx>,
                                                 inlining_map: &InliningMap<'tcx>,
                                                 local_usages: &mut FnvHashSet<TransItem<'tcx>>,
                                                 visited: &mut FnvHashSet<TransItem<'tcx>>) {
            if !visited.insert(trans_item) {
                return;
            }

            if let Some(used_by) = inlining_map.get(&trans_item) {
                for &usage in used_by {
                    if has_home_codegen_unit(usage) {
                        local_usages.insert(usage);
                    }

                    if propagates_usage(tcx, usage) {
                        collect_transitive_local_usages(usage,
                                                        tcx,
                                                        inlining_map,
                                                        local_usages,
                                                        visited);
                    }
                }
            }
        }

        fn propagates_usage<'tcx>(tcx: &TyCtxt<'tcx>,
                                  trans_item: TransItem<'tcx>)
                                  -> bool {
           trans_item.requests_inline(tcx) || trans_item.is_from_extern_crate()
        }

        fn has_home_codegen_unit<'tcx>(trans_item: TransItem<'tcx>) -> bool {
           !trans_item.is_from_extern_crate()
        }
    }
}

// This function tries to find an associated DefId for a given type. This DefId
// is used later to find the 'home' module for functions related to that type.
//
// There is a similar function in `ty::item_path` but it has slightly different
// goals (human-readable def-paths) so we are not going to re-use it.
fn characteristic_def_id_of_type<'tcx>(t: ty::Ty<'tcx>) -> Option<DefId> {
    match t.sty {
        ty::TyBool |
        ty::TyChar |
        ty::TyInt(_) |
        ty::TyUint(_) |
        ty::TyStr |
        ty::TyFnPtr(_) |
        ty::TyProjection(_) |
        ty::TyParam(_) |
        ty::TyInfer(_) |
        ty::TyError |
        ty::TyFloat(_) => None,

        ty::TyEnum(adt_def, _) |
        ty::TyStruct(adt_def, _) => Some(adt_def.did),

        ty::TyBox(ty) |
        ty::TyArray(ty, _) |
        ty::TySlice(ty) |
        ty::TyRawPtr(ty::TypeAndMut { ty, ..}) |
        ty::TyRef(_, ty::TypeAndMut { ty, ..}) => characteristic_def_id_of_type(ty),

        ty::TyClosure(def_id, _) |
        ty::TyFnDef(def_id, _, _) => Some(def_id),

        ty::TyTrait(ref trait_ty) => Some(trait_ty.principal_def_id()),

        ty::TyTuple(ref tys) => {
            tys.iter()
               .filter_map(|ty| characteristic_def_id_of_type(ty))
               .next()
        }
    }
}

fn is_fn(trans_item: TransItem) -> bool {
    if let TransItem::Fn(_) = trans_item {
        true
    } else {
        false
    }
}

impl<'tcx> CodegenUnit<'tcx> {
    pub fn _dump<'a>(&self, ccx: &CrateContext<'a, 'tcx>)
    {
        println!("CodegenUnit {} (", self.name);

        let mut items: Vec<_> = self.items.iter().map(|(trans_item, inst)| {
            format!("{} -- ({:?})", trans_item.to_string(ccx), inst)
        }).collect();

        items.as_mut_slice().sort();

        for s in items {
            println!("  {}", s);
        }

        println!(")");
    }
}

