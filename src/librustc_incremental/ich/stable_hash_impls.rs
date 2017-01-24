
use rustc::hir;
use rustc::dep_graph::StableHashingContext;
use rustc_data_structures::{HashStable, StableHasherResult, StableHasher};
use calculate_svh::svh_visitor::StrictVersionHashVisitor;


impl<'a, 'tcx> HashStable<StableHashingContext<'a, 'tcx>> for hir::Body {
    fn hash_stable<W: StableHasherResult>(&self,
                                          hcx: &StableHashingContext<'a, 'tcx>,
                                          hasher: &mut StableHasher<W>) {
        "HirBody".hash_stable(hcx, hasher);

        let mut hasher = IchHasher::new();
        let tcx = hcx.tcx;
        let mut def_path_hashes = DefPathHashes::new(tcx);

        StrictVersionHashVisitor::new(st: &'a mut IchHasher,
            //    tcx: TyCtxt<'hash, 'tcx, 'tcx>,
            //    def_path_hashes: &'a mut DefPathHashes<'hash, 'tcx>,
            //    codemap: &'a mut CachingCodemapView<'tcx>,
            //    hash_spans: bool,
            //    hash_bodies: bool)

    }
}
