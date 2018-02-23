// Copyright 2016 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use std::rc::Rc;
use std::sync::Arc;

use base;
use monomorphize::Instance;
use rustc::hir::def_id::CrateNum;
use rustc::hir::def_id::{DefId, LOCAL_CRATE};
// use rustc::middle::cstore::DepKind;
use rustc::middle::exported_symbols::SymbolExportLevel;
use rustc::session::config;
use rustc::ty::TyCtxt;
use rustc::ty::maps::Providers;
use rustc::util::nodemap::{FxHashMap};
use rustc_allocator::ALLOCATOR_METHODS;
use rustc_back::LinkerFlavor;
use syntax::attr;

pub type ExportedSymbols = FxHashMap<
    CrateNum,
    Arc<Vec<(String, Option<DefId>, SymbolExportLevel)>>,
>;

pub fn threshold(tcx: TyCtxt) -> SymbolExportLevel {
    crates_export_threshold(&tcx.sess.crate_types.borrow())
}

pub fn metadata_symbol_name(tcx: TyCtxt) -> String {
    format!("rust_metadata_{}_{}",
            tcx.crate_name(LOCAL_CRATE),
            tcx.crate_disambiguator(LOCAL_CRATE).to_fingerprint().to_hex())
}

fn crate_export_threshold(crate_type: config::CrateType) -> SymbolExportLevel {
    match crate_type {
        config::CrateTypeExecutable |
        config::CrateTypeStaticlib  |
        config::CrateTypeProcMacro  |
        config::CrateTypeCdylib     => SymbolExportLevel::C,
        config::CrateTypeRlib       |
        config::CrateTypeDylib      => SymbolExportLevel::Rust,
    }
}

pub fn crates_export_threshold(crate_types: &[config::CrateType])
                                      -> SymbolExportLevel {
    if crate_types.iter().any(|&crate_type| {
        crate_export_threshold(crate_type) == SymbolExportLevel::Rust
    }) {
        SymbolExportLevel::Rust
    } else {
        SymbolExportLevel::C
    }
}

pub fn provide(providers: &mut Providers) {
    providers.reachable_non_generics = |tcx, cnum| {
        let export_threshold = threshold(tcx);
        Rc::new(tcx.exported_symbols(cnum)
            .iter()
            .filter_map(|&(_, id, level)| {
                id.and_then(|id| {
                    if level.is_below_threshold(export_threshold) {
                        Some(id)
                    } else {
                        None
                    }
                })
            })
            .collect())
    };

    providers.is_reachable_non_generic = |tcx, id| {
        tcx.reachable_non_generics(id.krate).contains(&id)
    };

    providers.exported_symbols = |tcx, cnum| {
        assert_eq!(cnum, LOCAL_CRATE);
        let local_exported_symbols = base::find_exported_symbols(tcx);

        let mut local_crate: Vec<_> = local_exported_symbols
            .iter()
            .map(|&node_id| {
                tcx.hir.local_def_id(node_id)
            })
            .map(|def_id| {
                let name = tcx.symbol_name(Instance::mono(tcx, def_id));
                let export_level = export_level(tcx, def_id);
                debug!("EXPORTED SYMBOL (local): {} ({:?})", name, export_level);
                (str::to_owned(&name), Some(def_id), export_level)
            })
            .collect();

        if let Some(_) = *tcx.sess.entry_fn.borrow() {
            local_crate.push(("main".to_string(),
                              None,
                              SymbolExportLevel::C));
        }

        if tcx.sess.allocator_kind.get().is_some() {
            for method in ALLOCATOR_METHODS {
                local_crate.push((format!("__rust_{}", method.name),
                                  None,
                                  SymbolExportLevel::Rust));
            }
        }

        if let Some(id) = tcx.sess.derive_registrar_fn.get() {
            let def_id = tcx.hir.local_def_id(id);
            let disambiguator = tcx.sess.local_crate_disambiguator();
            let registrar = tcx.sess.generate_derive_registrar_symbol(disambiguator);
            local_crate.push((registrar, Some(def_id), SymbolExportLevel::C));
        }

        if tcx.sess.crate_types.borrow().contains(&config::CrateTypeDylib) {
            local_crate.push((metadata_symbol_name(tcx),
                              None,
                              SymbolExportLevel::Rust));
        }

        // Sort so we get a stable incr. comp. hash.
        local_crate.sort_unstable_by(|&(ref name1, ..), &(ref name2, ..)| {
            name1.cmp(name2)
        });

        Arc::new(local_crate)
    };

    providers.symbol_export_level = export_level;

    providers.available_monomorphizations = |tcx, cnum| {
        assert!(cnum == LOCAL_CRATE);

        // let all_crate_nums = tcx.all_crate_nums();

        // let size: usize = all_crate_nums.iter().map(|&cnum| {
        //     tcx.available_monomorphizations_in_crate(cnum).len()
        // }).sum();

        let crate_info = ::CrateInfo::new(tcx);

        let mut combined = FxHashMap();

        // ::back::link::each_linked_rlib(tcx.sess, &crate_info, &mut |cnum, _| {
        //     for &fp in tcx.available_monomorphizations_in_crate(cnum).iter() {
        //         combined.insert(fp, cnum);
        //     }
        // }).unwrap();

        for &(cnum, _) in crate_info.used_crates_static.iter() {
            assert!(cnum != LOCAL_CRATE);
            for &(fp, ref symbol_name) in tcx.available_monomorphizations_in_crate(cnum).iter() {
                combined.insert(fp, (symbol_name.clone(), cnum));
            }
        }





        // for &cnum in tcx.all_crate_nums(LOCAL_CRATE).iter() {
        //     match tcx.dep_kind(cnum) {
        //         DepKind::UnexportedMacrosOnly |
        //         DepKind::MacrosOnly => {
        //             continue
        //         }
        //         DepKind::Implicit |
        //         DepKind::Explicit => {}
        //     }

        //     for &fp in tcx.available_monomorphizations_in_crate(cnum).iter() {
        //         combined.insert(fp, cnum);
        //     }

        //     // combined.extend(tcx.available_monomorphizations_in_crate(cnum)
        //     //                    .iter()
        //     //                    .cloned());
        // }

        {
            use std::fs::File;
            use std::io::Write;

            let mut file = File::create(format!("available-monos-fp-{:?}.txt",
                tcx.original_crate_name(LOCAL_CRATE))).unwrap();

            for (&fp, &(ref symbol_name, cnum)) in combined.iter() {
                writeln!(file, "{:?} - {:?} - {}", fp, tcx.original_crate_name(cnum), symbol_name).unwrap();
            }
        }

        Rc::new(combined)
    };

    providers.available_monomorphization = |tcx, instance| {
        use rustc_data_structures::stable_hasher::{StableHasher, HashStable};

        let mut hasher = StableHasher::new();
        instance.hash_stable(&mut tcx.create_stable_hashing_context(), &mut hasher);
        let fingerprint = hasher.finish();

        tcx.available_monomorphizations(LOCAL_CRATE).get(&fingerprint).cloned()
    };

    providers.available_monomorphizations_in_crate = |tcx, cnum| {
        use rustc_data_structures::fx::FxHashSet;
        use rustc_data_structures::stable_hasher::{StableHasher, HashStable};
        use rustc::mir::mono::{MonoItem, Linkage, Visibility};
        use rustc::ty::InstanceDef;

        assert_eq!(cnum, LOCAL_CRATE);

        let codegen_units = tcx.collect_and_partition_translation_items(LOCAL_CRATE).1;
        let codegen_units = (*codegen_units).clone();

        let hcx = &mut tcx.create_stable_hashing_context();

        let available_monomorphizations: FxHashSet<_> = codegen_units
            .iter()
            .flat_map(|cgu| cgu.items().iter())
            .filter_map(|(&mono_item, &(linkage, visibility))| {
                if linkage == Linkage::External && visibility == Visibility::Default {
                    if let MonoItem::Fn(ref instance) = mono_item {
                        if let InstanceDef::Item(_) = instance.def {
                            if instance.substs.types().next().is_some() {
                                // return Some(*instance)
                                let symbol_name = tcx.symbol_name(*instance);
                                let mut hasher = StableHasher::new();
                                instance.hash_stable(hcx, &mut hasher);
                                let fingerprint = hasher.finish();

                                return Some((fingerprint, (&symbol_name.name[..]).to_owned()))
                            }
                        }
                    }
                }

                None
            })
            .collect();

        Rc::new(available_monomorphizations.into_iter().collect()) //<Vec<(Fingerprint, String)>
    };
}

pub fn provide_extern(providers: &mut Providers) {
    providers.exported_symbols = |tcx, cnum| {
        // If this crate is a plugin and/or a custom derive crate, then
        // we're not even going to link those in so we skip those crates.
        if tcx.plugin_registrar_fn(cnum).is_some() ||
           tcx.derive_registrar_fn(cnum).is_some() {
            return Arc::new(Vec::new())
        }

        // Check to see if this crate is a "special runtime crate". These
        // crates, implementation details of the standard library, typically
        // have a bunch of `pub extern` and `#[no_mangle]` functions as the
        // ABI between them. We don't want their symbols to have a `C`
        // export level, however, as they're just implementation details.
        // Down below we'll hardwire all of the symbols to the `Rust` export
        // level instead.
        let special_runtime_crate =
            tcx.is_panic_runtime(cnum) || tcx.is_compiler_builtins(cnum);

        // Dealing with compiler-builtins and wasm right now is super janky.
        // There's no linker! As a result we need all of the compiler-builtins
        // exported symbols to make their way through all the way to the end of
        // compilation. We want to make sure that LLVM doesn't remove them as
        // well because we may or may not need them in the final output
        // artifact. For now just force them to always get exported at the C
        // layer, and we'll worry about gc'ing them later.
        let compiler_builtins_and_binaryen =
            tcx.is_compiler_builtins(cnum) &&
            tcx.sess.linker_flavor() == LinkerFlavor::Binaryen;

        let mut crate_exports: Vec<_> = tcx
            .reachable_non_generics(cnum)
            .iter()
            .map(|&def_id| {
                let name = tcx.symbol_name(Instance::mono(tcx, def_id));
                let export_level = if compiler_builtins_and_binaryen &&
                                      tcx.contains_extern_indicator(def_id) {
                    SymbolExportLevel::C
                } else if special_runtime_crate {
                    // We can probably do better here by just ensuring that
                    // it has hidden visibility rather than public
                    // visibility, as this is primarily here to ensure it's
                    // not stripped during LTO.
                    //
                    // In general though we won't link right if these
                    // symbols are stripped, and LTO currently strips them.
                    if &*name == "rust_eh_personality" ||
                       &*name == "rust_eh_register_frames" ||
                       &*name == "rust_eh_unregister_frames" {
                        SymbolExportLevel::C
                    } else {
                        SymbolExportLevel::Rust
                    }
                } else {
                    export_level(tcx, def_id)
                };
                debug!("EXPORTED SYMBOL (re-export): {} ({:?})", name, export_level);
                (str::to_owned(&name), Some(def_id), export_level)
            })
            .collect();

        // Sort so we get a stable incr. comp. hash.
        crate_exports.sort_unstable_by(|&(ref name1, ..), &(ref name2, ..)| {
            name1.cmp(name2)
        });

        Arc::new(crate_exports)
    };

    providers.is_reachable_non_generic = |tcx, id| {
        tcx.reachable_non_generics(id.krate).contains(&id)
    };

    providers.symbol_export_level = export_level;
}

fn export_level(tcx: TyCtxt, sym_def_id: DefId) -> SymbolExportLevel {
    // We export anything that's not mangled at the "C" layer as it probably has
    // to do with ABI concerns. We do not, however, apply such treatment to
    // special symbols in the standard library for various plumbing between
    // core/std/allocators/etc. For example symbols used to hook up allocation
    // are not considered for export
    let is_extern = tcx.contains_extern_indicator(sym_def_id);
    let std_internal = attr::contains_name(&tcx.get_attrs(sym_def_id),
                                           "rustc_std_internal_symbol");
    if is_extern && !std_internal {
        SymbolExportLevel::C
    } else {
        SymbolExportLevel::Rust
    }
}
