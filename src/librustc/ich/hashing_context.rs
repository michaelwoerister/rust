

use rustc_data_structures::stable_hasher::{HashStable, StableHasher, StableHasherResult};
use ty;
use ich::DefPathHashes;
use hir;
use hir::def_id::DefId;
use std::collections::HashMap;
use std::hash as std_hash;
use std::mem;
use mir;
use util::caching_codemap_view::CachingCodemapView;
use syntax::ast;

pub struct StableHashingContext<'a, 'tcx: 'a> {
    tcx: ty::TyCtxt<'a, 'tcx, 'tcx>,
    def_path_hashes: DefPathHashes<'a, 'tcx>,
    codemap: CachingCodemapView<'tcx>,
    hash_spans: bool,
}

impl<'a, 'tcx: 'a> StableHashingContext<'a, 'tcx> {

    pub fn new(tcx: ty::TyCtxt<'a, 'tcx, 'tcx>) -> Self {
        StableHashingContext {
            tcx: tcx,
            def_path_hashes: DefPathHashes::new(tcx),
            codemap: CachingCodemapView::new(tcx),
            hash_spans: false, // TODO
        }
    }

    pub fn tcx(&self) -> ty::TyCtxt<'a, 'tcx, 'tcx> {
        self.tcx
    }

    pub fn def_path_hash(&mut self, def_id: DefId) -> u64 {
        self.def_path_hashes.hash(def_id)
    }

    pub fn hash_spans(&self) -> bool {
        self.hash_spans
    }

    pub fn codemap(&mut self) -> &mut CachingCodemapView<'tcx> {
        &mut self.codemap
    }
}

impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for ::syntax_pos::Span {

    // Hash a span in a stable way. We can't directly hash the span's BytePos
    // fields (that would be similar to hashing pointers, since those are just
    // offsets into the CodeMap). Instead, we hash the (file name, line, column)
    // triple, which stays the same even if the containing FileMap has moved
    // within the CodeMap.
    // Also note that we are hashing byte offsets for the column, not unicode
    // codepoint offsets. For the purpose of the hash that's sufficient.
    // Also, hashing filenames is expensive so we avoid doing it twice when the
    // span starts and ends in the same file, which is almost always the case.
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        use syntax_pos::Pos;

        if !hcx.hash_spans() {
            return
        }

        // If this is not an empty or invalid span, we want to hash the last
        // position that belongs to it, as opposed to hashing the first
        // position past it.
        let span_hi = if self.hi > self.lo {
            // We might end up in the middle of a multibyte character here,
            // but that's OK, since we are not trying to decode anything at
            // this position.
            self.hi - ::syntax_pos::BytePos(1)
        } else {
            self.hi
        };

{
        let loc1 = hcx.codemap().byte_pos_to_line_and_col(self.lo);
        let loc1 = loc1.as_ref()
                       .map(|&(ref fm, line, col)| (&fm.name[..], line, col.to_usize()))
                       .unwrap_or(("???", 0, 0));

        let loc2 = hcx.codemap().byte_pos_to_line_and_col(span_hi);
        let loc2 = loc2.as_ref()
                       .map(|&(ref fm, line, col)| (&fm.name[..], line, col.to_usize()))
                       .unwrap_or(("???", 0, 0));

        if loc1.0 == loc2.0 {
            std_hash::Hash::hash(&0u8, hasher);

            std_hash::Hash::hash(loc1.0, hasher);
            std_hash::Hash::hash(&loc1.1, hasher);
            std_hash::Hash::hash(&loc1.2, hasher);

            // Do not hash the file name twice
            std_hash::Hash::hash(&loc2.1, hasher);
            std_hash::Hash::hash(&loc2.2, hasher);
        } else {
            std_hash::Hash::hash(&1u8, hasher);

            std_hash::Hash::hash(loc1.0, hasher);
            std_hash::Hash::hash(&loc1.1, hasher);
            std_hash::Hash::hash(&loc1.2, hasher);

            std_hash::Hash::hash(loc2.0, hasher);
            std_hash::Hash::hash(&loc2.1, hasher);
            std_hash::Hash::hash(&loc2.2, hasher);
        }
}
        match self.expn_id {
            ::syntax_pos::NO_EXPANSION => {
                0u8.hash_stable(hcx, hasher);
            }
            ::syntax_pos::COMMAND_LINE_EXPN => {
                1u8.hash_stable(hcx, hasher);
            }
            _ => {
                2u8.hash_stable(hcx, hasher);

                let call_site = hcx.codemap().codemap().source_callsite(*self);
                call_site.hash_stable(hcx, hasher);
            }
        }
    }
}

impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for ::syntax::symbol::InternedString {
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        let s: &str = &**self;
        s.len().hash_stable(hcx, hasher);
        s.hash_stable(hcx, hasher);
    }
}

impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for ast::Name {
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        self.as_str().hash_stable(hcx, hasher);
    }
}

impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for DefId {
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        hcx.def_path_hash(*self).hash_stable(hcx, hasher);
    }
}

impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for ast::NodeId {
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        hcx.tcx.map.local_def_id(*self).hash_stable(hcx, hasher);
    }
}


impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for ty::Ty<'tcx> {
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        // let ty = hcx.tcx.erase_regions(self);
        let type_hash = hcx.tcx.type_id_hash(*self);
        debug!("hash_stable({:?}) = {}", *self, type_hash);
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

fn hash_stable_nodemap<'a, 'tcx, V, W>(hcx: &mut StableHashingContext<'a, 'tcx>,
                                       hasher: &mut StableHasher<W>,
                                       map: &::util::nodemap::NodeMap<V>)
    where V: HashStable<StableHashingContext<'a, 'tcx>>,
          W: StableHasherResult,
{
    let min = map.keys().min().map(ast::NodeId::as_u32).unwrap_or(0u32);

    hash_stable_hashmap(hcx, hasher, map, |_cx, node_id| {
        // let def_id = hcx.tcx().map.local_def_id(node_id);
        // hcx.def_path_hash(def_id)
        node_id.as_u32() - min
    });
}

// fn hash_stable_defidmap<'a, 'tcx, V, W>(hcx: &mut StableHashingContext<'a, 'tcx>,
//                                         hasher: &mut StableHasher<W>,
//                                         map: &::util::nodemap::DefIdMap<V>)
//     where V: HashStable<StableHashingContext<'a, 'tcx>>,
//           W: StableHasherResult,
// {
//     hash_stable_hashmap(hcx, hasher, map, |hcx, def_id| {
//         hcx.def_path_hash(def_id)
//     });
// }

// #[macro_export]
// macro_rules! __impl_stable_hash_field {
//     (DECL IGNORED) => (_);
//     (DECL $name:ident) => (ref $name);
//     (USE IGNORED $ctx:expr, $hasher:expr) => ({});
//     (USE $name:ident, $ctx:expr, $hasher:expr) => ($name.hash_stable($ctx, $hasher));
// }

// #[macro_export]
// macro_rules! impl_stable_hash_for {
//     (enum $enum_name:path { $( $variant:ident $( ( $($arg:ident),* ) )* ),* }) => {
//         impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for $enum_name {
//             fn hash_stable<W: StableHasherResult>(&self,
//                                                   __ctx: &mut StableHashingContext<'a, 'tcx>,
//                                                   __hasher: &mut StableHasher<W>) {
//                 use $enum_name::*;
//                 ::std::mem::discriminant(self).hash_stable(__ctx, __hasher);

//                 match *self {
//                     $(
//                         $variant $( ( $( __impl_stable_hash_field!(DECL $arg) ),* ) )* => {
//                             $($( __impl_stable_hash_field!(USE $arg, __ctx, __hasher) );*)*
//                         }
//                     )*
//                 }
//             }
//         }
//     };
//     (struct $struct_name:path { $($field:ident),* }) => {
//         impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for $struct_name {
//             fn hash_stable<W: StableHasherResult>(&self,
//                                                   __ctx: &mut StableHashingContext<'a, 'tcx>,
//                                                   __hasher: &mut StableHasher<W>) {
//                 let $struct_name {
//                     $(ref $field),*
//                 } = *self;

//                 $( $field.hash_stable(__ctx, __hasher));*
//             }
//         }
//     };
//     (tuple_struct $struct_name:path { $($field:ident),* }) => {
//         impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for $struct_name {
//             fn hash_stable<W: StableHasherResult>(&self,
//                                                   __ctx: &mut StableHashingContext<'a, 'tcx>,
//                                                   __hasher: &mut StableHasher<W>) {
//                 let $struct_name (
//                     $(ref $field),*
//                 ) = *self;

//                 $( $field.hash_stable(__ctx, __hasher));*
//             }
//         }
//     };
// }

impl_stable_hash_for!(enum hir::PrimTy {
    TyInt(t),
    TyUint(t),
    TyFloat(t),
    TyStr,
    TyBool,
    TyChar
});

impl_stable_hash_for!(enum hir::def::CtorKind {
    Fn,
    Const,
    Fictive
});

impl_stable_hash_for!(enum hir::def::Def {
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

impl_stable_hash_for!(struct ty::ItemSubsts<'tcx> { substs });

impl<'a, 'tcx, T> HashStable<StableHashingContext<'a, 'tcx>> for ty::Slice<T>
    where T: HashStable<StableHashingContext<'a, 'tcx>> {
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        (&**self).hash_stable(hcx, hasher);
    }
}

impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for ty::subst::Kind<'tcx> {
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        self.as_type().hash_stable(hcx, hasher);
        self.as_region().hash_stable(hcx, hasher);
    }
}

// Todo, use this in type id hasher
impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for ty::Region {
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        mem::discriminant(self).hash_stable(hcx, hasher);
        match *self {
            ty::ReEmpty |
            ty::ReStatic |
            ty::ReErased => {}
            ty::ReLateBound(db, ty::BrAnon(i)) => {
                assert!(db.depth > 0);
                db.depth.hash_stable(hcx, hasher);
                i.hash_stable(hcx, hasher);
            }
            ty::ReEarlyBound(ty::EarlyBoundRegion { index, name }) => {
                index.hash_stable(hcx, hasher);
                name.hash_stable(hcx, hasher);
            }
            ty::ReScope(..) => {
                // FIXME: Verify that this is true.
                // We don't do anything here, the contents should not matter.
            }
            ty::ReLateBound(..) |
            ty::ReFree(..) |
            ty::ReVar(..) |
            ty::ReSkolemized(..) => {
                bug!("HashStable::hash_stable: unexpected region {:?}", self)
            }
        }
    }
}

impl_stable_hash_for!(enum hir::Mutability {
    MutMutable,
    MutImmutable
});

impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for ty::adjustment::AutoBorrow<'tcx> {
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        mem::discriminant(self).hash_stable(hcx, hasher);
        match *self {
            ty::adjustment::AutoBorrow::Ref(ref region, mutability) => {
                region.hash_stable(hcx, hasher);
                mutability.hash_stable(hcx, hasher);
            }
            ty::adjustment::AutoBorrow::RawPtr(mutability) => {
                mutability.hash_stable(hcx, hasher);
            }
        }
    }
}

impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for ty::adjustment::Adjust<'tcx> {
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        mem::discriminant(self).hash_stable(hcx, hasher);
        match *self {
            ty::adjustment::Adjust::NeverToAny |
            ty::adjustment::Adjust::ReifyFnPointer |
            ty::adjustment::Adjust::UnsafeFnPointer |
            ty::adjustment::Adjust::MutToConstPointer => {}
            ty::adjustment::Adjust::DerefRef { autoderefs, ref autoref, unsize } => {
                autoderefs.hash_stable(hcx, hasher);
                autoref.hash_stable(hcx, hasher);
                unsize.hash_stable(hcx, hasher);
            }
        }
    }
}

impl_stable_hash_for!(struct ty::adjustment::Adjustment<'tcx> { kind, target });


impl_stable_hash_for!(struct ty::MethodCall { expr_id, autoderef });
impl_stable_hash_for!(struct ty::MethodCallee<'tcx> { def_id, ty, substs });
impl_stable_hash_for!(struct ty::UpvarId { var_id, closure_expr_id });
impl_stable_hash_for!(struct ty::UpvarBorrow<'tcx> { kind, region });

impl_stable_hash_for!(enum ty::BorrowKind {
    ImmBorrow,
    UniqueImmBorrow,
    MutBorrow
});


impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for ty::UpvarCapture<'tcx> {
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        mem::discriminant(self).hash_stable(hcx, hasher);
        match *self {
            ty::UpvarCapture::ByValue => {}
            ty::UpvarCapture::ByRef(ref up_var_borrow) => {
                up_var_borrow.hash_stable(hcx, hasher);
            }
        }
    }
}

impl_stable_hash_for!(struct ty::FnSig<'tcx> { inputs_and_output, variadic });

impl<'a, 'tcx, T> HashStable<StableHashingContext<'a, 'tcx>> for ty::Binder<T>
    where T: HashStable<StableHashingContext<'a, 'tcx>> + ty::fold::TypeFoldable<'tcx>
{
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        // self.0.hash_stable(hcx, hasher);
        // Anonymize late-bound regions so that, for example:
        // `for<'a, b> fn(&'a &'b T)` and `for<'a, b> fn(&'b &'a T)`
        // result in the same TypeId (the two types are equivalent).
        // hcx.tcx.erase_regions(self).0.hash_stable(hcx, hasher);
        hcx.tcx.anonymize_late_bound_regions(self).0.hash_stable(hcx, hasher);
    }
}

impl_stable_hash_for!(enum hir::Unsafety {
    Unsafe,
    Normal
});

impl_stable_hash_for!(enum ::syntax::abi::Abi {
    Cdecl,
    Stdcall,
    Fastcall,
    Vectorcall,
    Aapcs,
    Win64,
    SysV64,
    PtxKernel,
    Msp430Interrupt,
    Rust,
    C,
    System,
    RustIntrinsic,
    RustCall,
    PlatformIntrinsic,
    Unadjusted
});

impl_stable_hash_for!(struct ty::ClosureTy<'tcx> { unsafety, abi, sig });

impl_stable_hash_for!(enum ty::ClosureKind { Fn, FnMut, FnOnce });


impl<'tcx> ::std::fmt::Debug for ty::Tables<'tcx> {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "ty::Tables")
    }
}

impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for ty::Tables<'tcx> {
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        let ty::Tables {
            ref type_relative_path_defs,
            ref node_types,
            ref item_substs,
            ref adjustments,
            ref method_map,
            ref upvar_capture_map,
            ref closure_tys,
            ref closure_kinds,
            ref liberated_fn_sigs,
            ref fru_field_types
        } = *self;

        hash_stable_nodemap(hcx, hasher, type_relative_path_defs);
        hash_stable_nodemap(hcx, hasher, node_types);
        hash_stable_nodemap(hcx, hasher, item_substs);
        hash_stable_nodemap(hcx, hasher, adjustments);

        hash_stable_hashmap(hcx, hasher, method_map, |hcx, method_call| {
            let ty::MethodCall {
                expr_id,
                autoderef
            } = method_call;

            let def_id = hcx.tcx().map.local_def_id(expr_id);
            (hcx.def_path_hash(def_id), autoderef)
        });

        hash_stable_hashmap(hcx, hasher, upvar_capture_map, |hcx, up_var_id| {
            let ty::UpvarId {
                var_id,
                closure_expr_id
            } = up_var_id;

            let var_def_id = hcx.tcx().map.local_def_id(var_id);
            let closure_def_id = hcx.tcx().map.local_def_id(closure_expr_id);
            (hcx.def_path_hash(var_def_id), hcx.def_path_hash(closure_def_id))
        });

        hash_stable_nodemap(hcx, hasher, closure_tys);
        hash_stable_nodemap(hcx, hasher, closure_kinds);
        hash_stable_nodemap(hcx, hasher, liberated_fn_sigs);
        hash_stable_nodemap(hcx, hasher, fru_field_types);
    }
}

impl_stable_hash_for!(enum ty::Visibility {
    Public,
    Restricted(def_id),
    Invisible
});

impl_stable_hash_for!(struct ty::TraitRef<'tcx> { def_id, substs });
impl_stable_hash_for!(struct ty::TraitPredicate<'tcx> { trait_ref });
impl_stable_hash_for!(tuple_struct ty::EquatePredicate<'tcx> { t1, t2 });

impl<'a, 'tcx, A, B> HashStable<StableHashingContext<'a, 'tcx>> for ty::OutlivesPredicate<A, B>
    where A: HashStable<StableHashingContext<'a, 'tcx>>,
          B: HashStable<StableHashingContext<'a, 'tcx>>,
{
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        let ty::OutlivesPredicate(ref a, ref b) = *self;
        a.hash_stable(hcx, hasher);
        b.hash_stable(hcx, hasher);
    }
}

impl_stable_hash_for!(struct ty::ProjectionPredicate<'tcx> { projection_ty, ty });
impl_stable_hash_for!(struct ty::ProjectionTy<'tcx> { trait_ref, item_name });


impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for ty::Predicate<'tcx> {
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        mem::discriminant(self).hash_stable(hcx, hasher);
        match *self {
            ty::Predicate::Trait(ref pred) => {
                pred.hash_stable(hcx, hasher);
            }
            ty::Predicate::Equate(ref pred) => {
                pred.hash_stable(hcx, hasher);
            }
            ty::Predicate::RegionOutlives(ref pred) => {
                pred.hash_stable(hcx, hasher);
            }
            ty::Predicate::TypeOutlives(ref pred) => {
                pred.hash_stable(hcx, hasher);
            }
            ty::Predicate::Projection(ref pred) => {
                pred.hash_stable(hcx, hasher);
            }
            ty::Predicate::WellFormed(ty) => {
                ty.hash_stable(hcx, hasher);
            }
            ty::Predicate::ObjectSafe(def_id) => {
                def_id.hash_stable(hcx, hasher);
            }
            ty::Predicate::ClosureKind(def_id, closure_kind) => {
                def_id.hash_stable(hcx, hasher);
                closure_kind.hash_stable(hcx, hasher);
            }
        }
    }
}

impl_stable_hash_for!(struct ::syntax::attr::Deprecation { since, note });
impl_stable_hash_for!(struct ::syntax::attr::Stability { level, feature, rustc_depr });

impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for ::syntax::attr::StabilityLevel {
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        mem::discriminant(self).hash_stable(hcx, hasher);
        match *self {
            ::syntax::attr::StabilityLevel::Unstable { ref reason, ref issue } => {
                reason.hash_stable(hcx, hasher);
                issue.hash_stable(hcx, hasher);
            }
            ::syntax::attr::StabilityLevel::Stable { ref since } => {
                since.hash_stable(hcx, hasher);
            }
        }
    }
}

impl_stable_hash_for!(struct ::syntax::attr::RustcDeprecation { since, reason });



// MIR

impl_stable_hash_for!(struct mir::SourceInfo { span, scope });
impl_stable_hash_for!(enum mir::Mutability { Mut, Not });
impl_stable_hash_for!(enum mir::BorrowKind { Shared, Unique, Mut });
impl_stable_hash_for!(enum mir::LocalKind { Var, Temp, Arg, ReturnPointer });
impl_stable_hash_for!(struct mir::LocalDecl<'tcx> { mutability, ty, name, source_info });
impl_stable_hash_for!(struct mir::UpvarDecl { debug_name, by_ref });
impl_stable_hash_for!(struct mir::BasicBlockData<'tcx> { statements, terminator, is_cleanup });
impl_stable_hash_for!(struct mir::Terminator<'tcx> { source_info, kind });

impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for mir::Local {
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        use rustc_data_structures::indexed_vec::Idx;
        self.index().hash_stable(hcx, hasher);
    }
}

impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for mir::BasicBlock {
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        use rustc_data_structures::indexed_vec::Idx;
        self.index().hash_stable(hcx, hasher);
    }
}

impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for mir::Field {
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        use rustc_data_structures::indexed_vec::Idx;
        self.index().hash_stable(hcx, hasher);
    }
}

impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for mir::VisibilityScope {
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        use rustc_data_structures::indexed_vec::Idx;
        self.index().hash_stable(hcx, hasher);
    }
}

impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for mir::Promoted {
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        use rustc_data_structures::indexed_vec::Idx;
        self.index().hash_stable(hcx, hasher);
    }
}

impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for mir::TerminatorKind<'tcx> {
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        mem::discriminant(self).hash_stable(hcx, hasher);

        match *self {
            mir::TerminatorKind::Goto { ref target } => {
                target.hash_stable(hcx, hasher);
            }
            mir::TerminatorKind::If { ref cond, ref targets } => {
                cond.hash_stable(hcx, hasher);
                targets.hash_stable(hcx, hasher);
            }
            mir::TerminatorKind::Switch { ref discr, adt_def, ref targets } => {
                discr.hash_stable(hcx, hasher);
                adt_def.hash_stable(hcx, hasher);
                targets.hash_stable(hcx, hasher);
            }
            mir::TerminatorKind::SwitchInt { ref discr, switch_ty, ref values, ref targets } => {
                discr.hash_stable(hcx, hasher);
                switch_ty.hash_stable(hcx, hasher);
                values.hash_stable(hcx, hasher);
                targets.hash_stable(hcx, hasher);
            }
            mir::TerminatorKind::Resume |
            mir::TerminatorKind::Return |
            mir::TerminatorKind::Unreachable => {}
            mir::TerminatorKind::Drop { ref location, target, unwind } => {
                location.hash_stable(hcx, hasher);
                target.hash_stable(hcx, hasher);
                unwind.hash_stable(hcx, hasher);
            }
            mir::TerminatorKind::DropAndReplace { ref location, ref value, target, unwind, } => {
                location.hash_stable(hcx, hasher);
                value.hash_stable(hcx, hasher);
                target.hash_stable(hcx, hasher);
                unwind.hash_stable(hcx, hasher);
            }
            mir::TerminatorKind::Call { ref func, ref args, ref destination, cleanup } => {
                func.hash_stable(hcx, hasher);
                args.hash_stable(hcx, hasher);
                destination.hash_stable(hcx, hasher);
                cleanup.hash_stable(hcx, hasher);
            }
            mir::TerminatorKind::Assert { ref cond, expected, ref msg, target, cleanup } => {
                cond.hash_stable(hcx, hasher);
                expected.hash_stable(hcx, hasher);
                msg.hash_stable(hcx, hasher);
                target.hash_stable(hcx, hasher);
                cleanup.hash_stable(hcx, hasher);
            }
        }
    }
}

impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for mir::AssertMessage<'tcx> {
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        mem::discriminant(self).hash_stable(hcx, hasher);

        match *self {
            mir::AssertMessage::BoundsCheck { ref len, ref index } => {
                len.hash_stable(hcx, hasher);
                index.hash_stable(hcx, hasher);
            }
            mir::AssertMessage::Math(ref const_math_err) => {
                const_math_err.hash_stable(hcx, hasher);
            }
        }
    }
}

impl_stable_hash_for!(struct mir::Statement<'tcx> { source_info, kind });

impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for mir::StatementKind<'tcx> {
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        mem::discriminant(self).hash_stable(hcx, hasher);

        match *self {
            mir::StatementKind::Assign(ref lvalue, ref rvalue) => {
                lvalue.hash_stable(hcx, hasher);
                rvalue.hash_stable(hcx, hasher);
            }
            mir::StatementKind::SetDiscriminant { ref lvalue, variant_index } => {
                lvalue.hash_stable(hcx, hasher);
                variant_index.hash_stable(hcx, hasher);
            }
            mir::StatementKind::StorageLive(ref lvalue) |
            mir::StatementKind::StorageDead(ref lvalue) => {
                lvalue.hash_stable(hcx, hasher);
            }
            mir::StatementKind::Nop => {}
        }
    }
}

impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for mir::Lvalue<'tcx> {
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        mem::discriminant(self).hash_stable(hcx, hasher);
        match *self {
            mir::Lvalue::Local(ref local) => {
                local.hash_stable(hcx, hasher);
            }
            mir::Lvalue::Static(def_id) => {
                def_id.hash_stable(hcx, hasher);
            }
            mir::Lvalue::Projection(ref lvalue_projection) => {
                lvalue_projection.hash_stable(hcx, hasher);
            }
        }
    }
}

impl<'a, 'tcx, B, V> HashStable<StableHashingContext<'a, 'tcx>> for mir::Projection<'tcx, B, V>
    where B: HashStable<StableHashingContext<'a, 'tcx>>,
          V: HashStable<StableHashingContext<'a, 'tcx>>
{
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        let mir::Projection {
            ref base,
            ref elem,
        } = *self;

        base.hash_stable(hcx, hasher);
        elem.hash_stable(hcx, hasher);
    }
}

impl<'a, 'tcx, V> HashStable<StableHashingContext<'a, 'tcx>> for mir::ProjectionElem<'tcx, V>
    where V: HashStable<StableHashingContext<'a, 'tcx>>
{
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        mem::discriminant(self).hash_stable(hcx, hasher);
        match *self {
            mir::ProjectionElem::Deref => {}
            mir::ProjectionElem::Field(field, ty) => {
                field.hash_stable(hcx, hasher);
                ty.hash_stable(hcx, hasher);
            }
            mir::ProjectionElem::Index(ref value) => {
                value.hash_stable(hcx, hasher);
            }
            mir::ProjectionElem::ConstantIndex { offset, min_length, from_end } => {
                offset.hash_stable(hcx, hasher);
                min_length.hash_stable(hcx, hasher);
                from_end.hash_stable(hcx, hasher);
            }
            mir::ProjectionElem::Subslice { from, to } => {
                from.hash_stable(hcx, hasher);
                to.hash_stable(hcx, hasher);
            }
            mir::ProjectionElem::Downcast(adt_def, variant) => {
                adt_def.hash_stable(hcx, hasher);
                variant.hash_stable(hcx, hasher);
            }
        }
    }
}

impl_stable_hash_for!(struct mir::VisibilityScopeData { span, parent_scope });

impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for mir::Operand<'tcx> {
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        mem::discriminant(self).hash_stable(hcx, hasher);

        match *self {
            mir::Operand::Consume(ref lvalue) => {
                lvalue.hash_stable(hcx, hasher);
            }
            mir::Operand::Constant(ref constant) => {
                constant.hash_stable(hcx, hasher);
            }
        }
    }
}

impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for mir::Rvalue<'tcx> {
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        mem::discriminant(self).hash_stable(hcx, hasher);

        match *self {
            mir::Rvalue::Use(ref operand) => {
                operand.hash_stable(hcx, hasher);
            }
            mir::Rvalue::Repeat(ref operand, ref val) => {
                operand.hash_stable(hcx, hasher);
                val.hash_stable(hcx, hasher);
            }
            mir::Rvalue::Ref(region, borrow_kind, ref lvalue) => {
                region.hash_stable(hcx, hasher);
                borrow_kind.hash_stable(hcx, hasher);
                lvalue.hash_stable(hcx, hasher);
            }
            mir::Rvalue::Len(ref lvalue) => {
                lvalue.hash_stable(hcx, hasher);
            }
            mir::Rvalue::Cast(cast_kind, ref operand, ty) => {
                cast_kind.hash_stable(hcx, hasher);
                operand.hash_stable(hcx, hasher);
                ty.hash_stable(hcx, hasher);
            }
            mir::Rvalue::BinaryOp(op, ref operand1, ref operand2) |
            mir::Rvalue::CheckedBinaryOp(op, ref operand1, ref operand2) => {
                op.hash_stable(hcx, hasher);
                operand1.hash_stable(hcx, hasher);
                operand2.hash_stable(hcx, hasher);
            }
            mir::Rvalue::UnaryOp(op, ref operand) => {
                op.hash_stable(hcx, hasher);
                operand.hash_stable(hcx, hasher);
            }
            mir::Rvalue::Box(ty) => {
                ty.hash_stable(hcx, hasher);
            }
            mir::Rvalue::Aggregate(ref kind, ref operands) => {
                kind.hash_stable(hcx, hasher);
                operands.hash_stable(hcx, hasher);
            }
            mir::Rvalue::InlineAsm { ref asm, ref outputs, ref inputs } => {
                asm.hash_stable(hcx, hasher);
                outputs.hash_stable(hcx, hasher);
                inputs.hash_stable(hcx, hasher);
            }
        }
    }
}

impl_stable_hash_for!(enum mir::CastKind { Misc, ReifyFnPointer, UnsafeFnPointer, Unsize });

impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for mir::AggregateKind<'tcx> {
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        mem::discriminant(self).hash_stable(hcx, hasher);
        match *self {
            mir::AggregateKind::Array |
            mir::AggregateKind::Tuple => {}
            mir::AggregateKind::Adt(adt_def, idx, substs, active_field) => {
                adt_def.hash_stable(hcx, hasher);
                idx.hash_stable(hcx, hasher);
                substs.hash_stable(hcx, hasher);
                active_field.hash_stable(hcx, hasher);
            }
            mir::AggregateKind::Closure(def_id, ref substs) => {
                def_id.hash_stable(hcx, hasher);
                substs.hash_stable(hcx, hasher);
            }
        }
    }
}

impl_stable_hash_for!(enum mir::BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    BitXor,
    BitAnd,
    BitOr,
    Shl,
    Shr,
    Eq,
    Lt,
    Le,
    Ne,
    Ge,
    Gt
});

impl_stable_hash_for!(enum mir::UnOp {
    Not,
    Neg
});


impl_stable_hash_for!(struct mir::Constant<'tcx> { span, ty, literal });
impl_stable_hash_for!(struct mir::TypedConstVal<'tcx> { ty, span, value });


impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for mir::Literal<'tcx> {
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        mem::discriminant(self).hash_stable(hcx, hasher);
        match *self {
            mir::Literal::Item { def_id, substs } => {
                def_id.hash_stable(hcx, hasher);
                substs.hash_stable(hcx, hasher);
            }
            mir::Literal::Value { ref value } => {
                value.hash_stable(hcx, hasher);
            }
            mir::Literal::Promoted { index } => {
                index.hash_stable(hcx, hasher);
            }
        }
    }
}

impl_stable_hash_for!(struct mir::Location { block, statement_index });



impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for ty::AdtFlags {
    fn hash_stable<W: StableHasherResult>(&self,
                                          _: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        std_hash::Hash::hash(self, hasher);
    }
}

impl_stable_hash_for!(struct ty::VariantDef {
    did,
    name,
    disr_val,
    fields,
    ctor_kind
});

impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for ty::FieldDef {
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        let ty::FieldDef {
            did,
            name,
            vis
        } = *self;

        did.hash_stable(hcx, hasher);
        name.hash_stable(hcx, hasher);
        vis.hash_stable(hcx, hasher);

        //variants.get().hash_stable(hcx, hasher);
        // hcx.tcx.item_type(did).hash_stable(hcx, hasher);
    }
}

// impl_stable_hash_for!(struct ty::FieldDef {
//     did,
//     name,
//     vis,
// });


impl_stable_hash_for!(enum ::middle::const_val::ConstVal {
    Float(const_float),
    Integral(const_int),
    Str(val),
    ByteStr(val),
    Bool(val),
    Function(def_id),
    Struct(val), // BTreeMap<ast::Name, ConstVal>),
    Tuple(val),
    Array(val),
    Repeat(val, count),
    Char(val)
});

impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for ::rustc_const_math::ConstFloat {
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        mem::discriminant(self).hash_stable(hcx, hasher);

        match *self {
            ::rustc_const_math::ConstFloat::F32(val) => {
                val.hash_stable(hcx, hasher);
            }
            ::rustc_const_math::ConstFloat::F64(val) => {
                val.hash_stable(hcx, hasher);
            }
            ::rustc_const_math::ConstFloat::FInfer { f32: v32, f64: v64 } => {
                v32.hash_stable(hcx, hasher);
                v64.hash_stable(hcx, hasher);
            }
        }
    }
}

impl_stable_hash_for!(enum ::rustc_const_math::ConstInt {
    I8(val),
    I16(val),
    I32(val),
    I64(val),
    I128(val),
    Isize(val),
    U8(val),
    U16(val),
    U32(val),
    U64(val),
    U128(val),
    Usize(val),
    Infer(val),
    InferSigned(val)
});

impl_stable_hash_for!(enum ::rustc_const_math::ConstIsize {
    Is16(i16),
    Is32(i32),
    Is64(i64)
});

impl_stable_hash_for!(enum ::rustc_const_math::ConstUsize {
    Us16(i16),
    Us32(i32),
    Us64(i64)
});

impl_stable_hash_for!(enum ::rustc_const_math::ConstMathErr {
    NotInRange,
    CmpBetweenUnequalTypes,
    UnequalTypes(op),
    Overflow(op),
    ShiftNegative,
    DivisionByZero,
    RemainderByZero,
    UnsignedNegation,
    ULitOutOfRange(int_ty),
    LitOutOfRange(int_ty)
});

impl_stable_hash_for!(enum ::rustc_const_math::Op {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Shr,
    Shl,
    Neg,
    BitAnd,
    BitOr,
    BitXor
});


impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for hir::InlineAsm {
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        "InlineAsm".hash_stable(hcx, hasher);
    }
}

impl_stable_hash_for!(struct ty::ClosureSubsts<'tcx> { substs });



macro_rules! impl_stable_hash_for_spanned {
    ($T:path) => (

        impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for ::syntax::codemap::Spanned<$T>
        {
            fn hash_stable<W: StableHasherResult>(&self,
                                                  hcx: &mut StableHashingContext<'a, 'tcx>,
                                                  hasher: &mut StableHasher<W>) {
                self.node.hash_stable(hcx, hasher);
                self.span.hash_stable(hcx, hasher);
            }
        }
    );
}


impl_stable_hash_for!(enum ::syntax::ast::LitIntType {
    Signed(int_ty),
    Unsigned(int_ty),
    Unsuffixed
});

impl_stable_hash_for_spanned!(::syntax::ast::LitKind);
impl_stable_hash_for!(enum ::syntax::ast::LitKind {
    Str(value, style),
    ByteStr(value),
    Byte(value),
    Char(value),
    Int(value, lit_int_type),
    Float(value, float_ty),
    FloatUnsuffixed(value),
    Bool(value)
});

impl_stable_hash_for!(enum ::syntax::ast::IntTy { Is, I8, I16, I32, I64, I128 });
impl_stable_hash_for!(enum ::syntax::ast::UintTy { Us, U8, U16, U32, U64, U128 });
impl_stable_hash_for!(enum ::syntax::ast::FloatTy { F32, F64 });
impl_stable_hash_for!(enum ::syntax::ast::Unsafety { Unsafe, Normal });
impl_stable_hash_for!(enum ::syntax::ast::Constness { Const, NotConst });
impl_stable_hash_for!(enum ::syntax::ast::Defaultness { Default, Final });
impl_stable_hash_for!(struct ::syntax::ast::Lifetime { id, span, name });
impl_stable_hash_for!(struct ::syntax::ast::LifetimeDef { attrs, lifetime, bounds });
impl_stable_hash_for!(enum ::syntax::ast::StrStyle { Cooked, Raw(pounds) });
impl_stable_hash_for!(enum ::syntax::ast::AttrStyle { Outer, Inner });

impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for
    ::syntax::util::ThinVec<::syntax::ast::Attribute>
{
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        // Do not hash length here
        for attribute in self.iter() {
            attribute.hash_stable(hcx, hasher);
        }
        // (&**self).hash_stable(hcx, hasher);
    }
}

impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for ast::MetaItem {
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        // ignoring span information, it doesn't matter here
        mem::discriminant(self).hash_stable(hcx, hasher);

        self.name.hash_stable(hcx, hasher);

        match self.node {
            ast::MetaItemKind::Word => {}
            ast::MetaItemKind::NameValue(ref lit) => {
                lit.hash_stable(hcx, hasher);
            }
            ast::MetaItemKind::List(ref items) => {
                // Sort subitems so the hash does not depend on their order
                let indices = indices_sorted_by(&items, |p| {
                    // TODO: doesn't handle literals
                    p.name().map(::syntax::symbol::Symbol::as_str)
                });
                items.len().hash_stable(hcx, hasher);
                for &index in indices.iter() {
                    let nested_meta_item: &ast::NestedMetaItemKind = &items[index].node;
                    mem::discriminant(nested_meta_item).hash_stable(hcx, hasher);
                    match *nested_meta_item {
                        ast::NestedMetaItemKind::MetaItem(ref meta_item) => {
                            meta_item.hash_stable(hcx, hasher);
                        }
                        ast::NestedMetaItemKind::Literal(ref lit) => {
                            lit.hash_stable(hcx, hasher);
                        }
                    }
                }
            }
        }
    }
}

impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for ast::Attribute {
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        if !self.is_sugared_doc &&
           !super::IGNORED_ATTRIBUTES.contains(&&*self.value.name().as_str()) {
            self.style.hash_stable(hcx, hasher);
            self.value.hash_stable(hcx, hasher);
        }
    }
}


fn indices_sorted_by<T, K, F>(items: &[T], get_key: F) -> Vec<usize>
    where K: Ord,
          F: Fn(&T) -> K
{
    let mut indices = Vec::with_capacity(items.len());
    indices.extend(0 .. items.len());
    indices.sort_by_key(|index| get_key(&items[*index]));
    indices
}


impl_stable_hash_for!(enum ::hir::Constness {
    Const,
    NotConst
});

impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for ::hir::def_id::DefIndex {

    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        DefId::local(*self).hash_stable(hcx, hasher);
    }
}

impl_stable_hash_for!(struct ty::GenericPredicates<'tcx> {
    parent,
    predicates
});

impl_stable_hash_for!(enum hir::ImplPolarity {
    Positive,
    Negative
});

impl_stable_hash_for!(struct hir::def::Export { name, def });

impl_stable_hash_for!(enum ty::Variance {
    Covariant,
    Invariant,
    Contravariant,
    Bivariant
});

impl_stable_hash_for!(enum ty::adjustment::CustomCoerceUnsized {
    Struct(index)
});

impl_stable_hash_for!(struct ty::Generics<'tcx> {
    parent,
    parent_regions,
    parent_types,
    regions,
    types,
    has_self
});

// impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for ty::Generics<'tcx> {
//     fn hash_stable<W: StableHasherResult>(&self,
//                                           hcx: &mut StableHashingContext<'a, 'tcx>,
//                                           hasher: &mut StableHasher<W>) {
//         let generics = hcx.tcx.erase_regions(self);

//         let ty::Generics {
//             parent,
//             ref parent_regions,
//             ref parent_types,
//             ref regions,
//             ref types,
//             has_self
//         } = generics;

//         parent.hash_stable(hcx, hasher);
//         parent_regions.hash_stable(hcx, hasher);
//         parent_types.hash_stable(hcx, hasher);
//         regions.hash_stable(hcx, hasher);
//         types.hash_stable(hcx, hasher);
//         has_self.hash_stable(hcx, hasher);
//     }
// }

impl_stable_hash_for!(struct ty::RegionParameterDef<'tcx> {
    name,
    def_id,
    index,
    bounds,
    pure_wrt_drop
});

impl_stable_hash_for!(struct ty::TypeParameterDef<'tcx> {
    name,
    def_id,
    index,
    default_def_id,
    default,
    object_lifetime_default,
    pure_wrt_drop
});

impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for ty::ObjectLifetimeDefault<'tcx> {
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        mem::discriminant(self).hash_stable(hcx, hasher);
        match *self {
            ty::ObjectLifetimeDefault::Ambiguous |
            ty::ObjectLifetimeDefault::BaseDefault => {}
            ty::ObjectLifetimeDefault::Specific(region) => {
                region.hash_stable(hcx, hasher);
            }
        }
    }
}

impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for hir::Body {
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &mut StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        // pub struct Body {
        //     pub arguments: HirVec<Arg>,
        //     pub value: Expr
        // }

        self.arguments.len().hash_stable(hcx, hasher);

        let StableHashingContext {
            tcx,
            ref mut def_path_hashes,
            ref mut codemap,
            hash_spans,
            ..
        } = *hcx;

        let mut hir_hasher = super::StrictVersionHashVisitor::new(hasher,
               tcx,
               def_path_hashes,
               codemap,
               hash_spans,
               true);

        // TODO: Arg
        // for arg in self.arguments.iter() {
        //     hir
        // }

        let expr: &'tcx hir::Expr = unsafe {
            mem::transmute(&self.value)
        };

        hir::intravisit::Visitor::visit_expr(&mut hir_hasher, &expr);
    }
}
