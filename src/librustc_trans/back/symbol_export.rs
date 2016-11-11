// Copyright 2016 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use context::SharedCrateContext;
use monomorphize::Instance;
use symbol_map::SymbolMap;
use rustc_data_structures::fnv::FnvHashMap;
use rustc::hir::def_id::{DefId, CrateNum, LOCAL_CRATE};
use rustc::session::config;
use syntax::attr;
use trans_item::TransItem;

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub enum SymbolExportLevel {
    AnyLib,
    RustDylibOnly,
}

pub struct ExportedSymbols {
    exports: FnvHashMap<CrateNum, Vec<(String, SymbolExportLevel)>>,
}

impl ExportedSymbols {

    pub fn empty() -> ExportedSymbols {
        ExportedSymbols {
            exports: FnvHashMap(),
        }
    }

    pub fn compute_from<'a, 'tcx>(scx: &SharedCrateContext<'a, 'tcx>,
                                  symbol_map: &SymbolMap<'tcx>)
                                  -> ExportedSymbols {
        let mut local_crate: Vec<_> = scx
            .exported_symbols()
            .iter()
            .map(|&node_id| {
                scx.tcx().map.local_def_id(node_id)
            })
            .map(|def_id| {
                (symbol_for_def_id(scx, def_id, symbol_map),
                 export_level(scx, def_id))
            })
            .collect();

        if scx.sess().entry_fn.borrow().is_some() {
            local_crate.push(("main".to_string(), SymbolExportLevel::AnyLib));
        }

        if scx.sess().crate_types.borrow().contains(&config::CrateTypeDylib) {
            local_crate.push((scx.metadata_symbol_name(),
                              SymbolExportLevel::AnyLib));
        }

        let mut exports = FnvHashMap();
        exports.insert(LOCAL_CRATE, local_crate);

        for cnum in scx.sess().cstore.crates() {
            debug_assert!(cnum != LOCAL_CRATE);

            let crate_exports = scx
                .sess()
                .cstore
                .exported_symbols(cnum)
                .iter()
                .map(|&def_id| {
                    let name = Instance::mono(scx, def_id).symbol_name(scx);
                    (name, export_level(scx, def_id))
                })
                .collect();

            exports.insert(cnum, crate_exports);
        }

        return ExportedSymbols {
            exports: exports
        };

        fn export_level(scx: &SharedCrateContext,
                        sym_def_id: DefId)
                        -> SymbolExportLevel {
            let attrs = scx.tcx().get_attrs(sym_def_id);
            if attr::contains_extern_indicator(scx.sess().diagnostic(), &attrs) {
                SymbolExportLevel::AnyLib
            } else {
                SymbolExportLevel::RustDylibOnly
            }
        }
    }

    pub fn exported_symbols(&self,
                            cnum: CrateNum)
                            -> &[(String, SymbolExportLevel)] {
        &(self.exports[&cnum])[..]
    }

    pub fn for_each_exported_symbol<F>(&self,
                                       cnum: CrateNum,
                                       max_export_level: SymbolExportLevel,
                                       mut f: F)
        where F: FnMut(&str, SymbolExportLevel)
    {
        for &(ref name, export_level) in self.exports[&cnum].iter() {
            if export_level.less_or_equal(max_export_level) {
                f(&name[..], export_level)
            }
        }
    }
}

pub fn symbol_export_level(crate_type: config::CrateType) -> SymbolExportLevel {
    match crate_type {
        config::CrateTypeExecutable |
        config::CrateTypeStaticlib  |
        config::CrateTypeCdylib     => SymbolExportLevel::AnyLib,
        config::CrateTypeProcMacro  |
        config::CrateTypeRlib       |
        config::CrateTypeDylib      => SymbolExportLevel::RustDylibOnly,
    }
}

pub fn symbol_export_threshold(crate_types: &[config::CrateType])
                               -> SymbolExportLevel {
    for &crate_type in crate_types {
        if symbol_export_level(crate_type) == SymbolExportLevel::RustDylibOnly {
            return SymbolExportLevel::RustDylibOnly;
        }
    }

    SymbolExportLevel::AnyLib
}

impl SymbolExportLevel {
    pub fn less_or_equal(&self, other: SymbolExportLevel) -> bool {
        if *self == SymbolExportLevel::AnyLib {
            true
        } else {
            other == SymbolExportLevel::RustDylibOnly
        }
    }
}


fn symbol_for_def_id<'a, 'tcx>(scx: &SharedCrateContext<'a, 'tcx>,
                               def_id: DefId,
                               symbol_map: &SymbolMap<'tcx>)
                               -> String {
    // Just try to look things up in the symbol map. If nothing's there, we
    // recompute.
    if let Some(node_id) = scx.tcx().map.as_local_node_id(def_id) {
        if let Some(sym) = symbol_map.get(TransItem::Static(node_id)) {
            return sym.to_owned();
        }
    }

    let instance = Instance::mono(scx, def_id);

    symbol_map.get(TransItem::Fn(instance))
              .map(str::to_owned)
              .unwrap_or_else(|| instance.symbol_name(scx))
}
