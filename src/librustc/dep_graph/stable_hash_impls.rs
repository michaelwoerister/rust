

use rustc_data_structures::stable_hasher::{HashStable, StableHasher, StableHasherResult};
use ty::TyCtxt;
use dep_graph::DefPathHashes;
use hir::def_id::DefId;
use hir::def::Def;
use std::collections::HashMap;
use std::mem;
use std::hash as std_hash;

pub struct StableHashingContext<'a, 'tcx: 'a> {
    tcx: TyCtxt<'a, 'tcx, 'tcx>,
    def_path_hashes: DefPathHashes<'a, 'tcx>,
}

impl<'a, 'tcx: 'a> StableHashingContext<'a, 'tcx> {

    pub fn tcx(&self) -> TyCtxt<'a, 'tcx, 'tcx> {
        self.tcx
    }

    pub fn def_path_hash(&mut self, def_id: DefId) -> u64 {
        self.def_path_hashes.hash(def_id)
    }

}

impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for ::syntax_pos::Span {
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        "span".hash_stable(hcx, hasher);
    }
}

impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for DefId {
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        hcx.def_path_hash(*self).hash_stable(hcx, hasher);
    }
}

impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for ::syntax::ast::NodeId {
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        hcx.tcx.map.local_def_id(*self).hash_stable(hcx, hasher);
    }
}


impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for ::ty::Ty<'tcx> {
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        let type_hash = hcx.tcx.type_id_hash(*self);
        type_hash.hash_stable(hcx, hasher);
    }
}

fn hash_stable_hashmap<'a, 'tcx, K, V, R, SK, F, W>(hcx: &mut StableHashingContext<'a, 'tcx>,
                                                    hasher: &mut StableHasher<W>,
                                                    map: &HashMap<K, V, R>,
                                                    extract_stable_key: F)
    where K: Clone + Eq + std_hash::Hash,
          V: HashStable<StableHashingContext<'a, 'tcx>>,
          R: std_hash::BuildHasher,
          SK: HashStable<StableHashingContext<'a, 'tcx>> + Ord + Clone,
          F: Fn(&mut StableHashingContext<'a, 'tcx>, K) -> SK,
          W: StableHasherResult,
{
    let mut keys: Vec<_> = map.keys()
                              .cloned()
                              .map(|k| (extract_stable_key(hcx, k.clone()), k))
                              .collect();
    keys.sort_by_key(|&(ref stable_key, _)| stable_key.clone());
    keys.len().hash_stable(hcx, hasher);
    for (hash, key) in keys {
        hash.hash_stable(hcx, hasher);
        map[&key].hash_stable(hcx, hasher);
    }
}

fn hash_stable_nodemap<'a, 'tcx, V, F, W>(hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>,
                                          map: &::util::nodemap::NodeMap<V>)
    where V: HashStable<StableHashingContext<'a, 'tcx>>,
          W: StableHasherResult,
{
    hash_stable_hashmap(hcx, hasher, map, |hcx, node_id| {
        let def_id = hcx.tcx().map.local_def_id(node_id);
        hcx.def_path_hash(def_id)
    });
}

fn hash_stable_defidmap<'a, 'tcx, V, F, W>(hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>,
                                          map: &::util::nodemap::DefIdMap<V>)
    where V: HashStable<StableHashingContext<'a, 'tcx>>,
          W: StableHasherResult,
{
    hash_stable_hashmap(hcx, hasher, map, |hcx, def_id| {
        hcx.def_path_hash(def_id)
    });
}

// macro_rules! impl_enum_stable_hash_for {
//     (enum $enum_name:ty { $( $variant:ident $( ( $($name:ident),* ) )*   ),* }) => {
//         impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for $enum_name {
//             fn hash_stable<W: StableHasherResult>(&self,
//                                                   __ctx: &mut StableHashingContext<'a, 'tcx>,
//                                                   __hasher: &mut StableHasher<W>) {
//                 ::std::hash::Hash::hash(&::std::mem::discriminant(self), __hasher);
//                 match *self {
//                     $(
//                         $enum_name::$variant $( ( $(ref $name),* ) )* => {
//                             $($($name.hash_stable(__ctx, __hasher);)*)*
//                         }
//                     )*
//                 }
//             }
//         }
//     }
// }


macro_rules! __impl_stable_hash_field {
    (DECL IGNORED) => (_);
    (DECL $name:ident) => (ref $name);
    (USE IGNORED $ctx:expr, $hasher:expr) => ({});
    (USE $name:ident, $ctx:expr, $hasher:expr) => ($name.hash_stable($ctx, $hasher));
}

macro_rules! impl_enum_stable_hash_for {
    (enum $enum_name:path { $( $variant:ident $( ( $($arg:ident),* ) )* ),* }) => {
        impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for $enum_name {
            fn hash_stable<W: StableHasherResult>(&self,
                                                  __ctx: &mut StableHashingContext<'a, 'tcx>,
                                                  __hasher: &mut StableHasher<W>) {
                use $enum_name::*;
                ::std::hash::Hash::hash(&::std::mem::discriminant(self), __hasher);

                match *self {
                    $(
                        $variant $( ( $( __impl_stable_hash_field!(DECL $arg) ),* ) )* => {
                            $($( __impl_stable_hash_field!(USE $arg, __ctx, __hasher) );*)*
                        }
                    )*
                }
            }
        }
    }
}

impl_enum_stable_hash_for!(enum ::syntax::ast::IntTy {
    Is,
    I8,
    I16,
    I32,
    I64,
    I128
});

impl_enum_stable_hash_for!(enum ::syntax::ast::UintTy {
    Us,
    U8,
    U16,
    U32,
    U64,
    U128
});

impl_enum_stable_hash_for!(enum ::syntax::ast::FloatTy {
    F32,
    F64
});

impl_enum_stable_hash_for!(enum ::hir::PrimTy {
    TyInt(t),
    TyUint(t),
    TyFloat(t),
    TyStr,
    TyBool,
    TyChar
});

impl_enum_stable_hash_for!(enum ::hir::def::CtorKind {
    Fn,
    Const,
    Fictive
});

impl_enum_stable_hash_for!(enum ::hir::def::Def {
    Mod(def_id),
    Struct(def_id),
    Union(def_id),
    Enum(def_id),
    Variant(def_id),
    Trait(def_id),
    TyAlias(def_id),
    AssociatedTy(def_id),
    PrimTy(prim_ty),
    TyParam(def_id),
    SelfTy(trait_def_id, impl_def_id),
    Fn(def_id),
    Const(def_id),
    Static(def_id, is_mutbl),
    StructCtor(def_id, ctor_kind),
    VariantCtor(def_id, ctor_kind),
    Method(def_id),
    AssociatedConst(def_id),
    Local(def_id),
    Upvar(def_id, index, expr_id),
    Label(node_id),
    Macro(def_id),
    Err
});

// // Macros 1.1 plz
// impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for Def {
//     fn hash_stable<W: StableHasherResult>(&self,
//                                           hcx: &mut StableHashingContext<'a, 'tcx>,
//                                           hasher: &mut StableHasher<W>) {
//     std_hash::Hash::hash(&mem::discriminant(self), hasher);
//     match *self {
//         Def::Mod(def_id) |
//         Def::Struct(def_id) |
//         Def::Union(def_id) |
//         Def::Enum(def_id) |
//         Def::Variant(def_id) |
//         Def::Trait(def_id) |
//         Def::TyAlias(def_id) |
//         Def::AssociatedTy(def_id) |
//         Def::TyParam(def_id) |
//         Def::Fn(def_id) |
//         Def::Const(def_id) |
//         Def::Method(def_id) |
//         Def::AssociatedConst(def_id) |
//         Def::Local(def_id) |
//         Def::Macro(def_id) => def_id.hash_stable(hcx, hasher),
//         Def::PrimTy(prim_ty) => prim_ty.hash_stable(hcx, hasher),
//         Def::SelfTy(trait_def_id, impl_def_id) => {
//             trait_def_id.hash_stable(hcx, hasher);
//             impl_def_id.hash_stable(hcx, hasher);
//         }
//         Def::Static(def_id, is_mutbl) => {
//             def_id.hash_stable(hcx, hasher);
//             is_mutbl.hash_stable(hcx, hasher);
//         }
//         Def::StructCtor(def_id, ctor_kind) |
//         Def::VariantCtor(def_id, ctor_kind) => {
//             def_id.hash_stable(hcx, hasher);
//             ctor_kind.hash_stable(hcx, hasher);
//         }
//         Def::Upvar(def_id, index, expr_id) => {
//             def_id.hash_stable(hcx, hasher);
//             index.hash_stable(hcx, hasher);
//             expr_id.hash_stable(hcx, hasher);
//         }
//         Def::Label(node_id) => node_id.hash_stable(hcx, hasher),
//         Err => { /* nothing to do */ }
//     }
// }


