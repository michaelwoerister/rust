// Copyright 2017 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use dep_graph::{DepNodeIndex, SerializedDepNodeIndex};
use errors::Diagnostic;
use hir;
use hir::def_id::{DefId, DefIndex, CrateNum, LOCAL_CRATE, INCR_CACHE_CRATE};
use hir::map::definitions::{Definitions, DefPathTable};
use middle::const_val::ByteArray;
use middle::cstore::CrateStore;
use mir::Mir;
use rustc_data_structures::fx::FxHashMap;
use rustc_data_structures::indexed_vec::{IndexVec, Idx};
use rustc_serialize::{Decodable, Decoder, Encodable, Encoder, opaque,
                      SpecializedDecoder, SpecializedEncoder};
use session::{Session, CrateDisambiguator};
use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::mem;
use syntax::ast::NodeId;
use syntax::codemap::{CodeMap, StableFilemapId};
use syntax_pos::{BytePos, Span, NO_EXPANSION, DUMMY_SP};
use ty::{self, Ty, TyCtxt};
use ty::codec as ty_codec;
use ty::subst::Substs;
// const HEADER_TAG: u64 = 0x1234_5678_FFFF_FFFF;
const PREV_DIAGNOSTICS_TAG: u64 = 0x1234_5678_FFFF_EEEE;
const QUERY_RESULT_INDEX_TAG: u64 = 0x1234_5678_DDDD_DDDD;
const QUERY_RESULT_ICH_TAG: u64 = 0x1234_5678_CCCC_CCCC;
const DEF_PATH_TABLE_TAG: u64 = 0x1234_5678_BB00_0000;

/// `OnDiskCache` provides an interface to incr. comp. data cached from the
/// previous compilation session. This data will eventually include the results
/// of a few selected queries (like `typeck_tables_of` and `mir_optimized`) and
/// any diagnostics that have been emitted during a query.
pub struct OnDiskCache<'sess> {

    serialized_data: Vec<u8>,

    // The diagnostics emitted during the previous compilation session.
    prev_diagnostics: FxHashMap<SerializedDepNodeIndex, Vec<Diagnostic>>,

    // This field collects all Diagnostics emitted during the current
    // compilation session.
    current_diagnostics: RefCell<FxHashMap<DepNodeIndex, Vec<Diagnostic>>>,

    // This will eventually be needed for creating Decoders that can rebase
    // spans.
    prev_filemap_starts: BTreeMap<BytePos, StableFilemapId>,
    codemap: &'sess CodeMap,

    prev_cnums: Vec<(u32, String, CrateDisambiguator)>,
    cnum_map: RefCell<Option<IndexVec<CrateNum, Option<CrateNum>>>>,

    prev_def_path_tables: Vec<DefPathTable>,

    query_result_index: FxHashMap<SerializedDepNodeIndex, usize>,
}

// This type is used only for (de-)serialization.
#[derive(RustcEncodable, RustcDecodable, Clone)]
struct Header {
    prev_filemap_starts: BTreeMap<BytePos, StableFilemapId>,
    prev_cnums: Vec<(u32, String, CrateDisambiguator)>,
}

type EncodedPrevDiagnostics = Vec<(SerializedDepNodeIndex, Vec<Diagnostic>)>;

struct FixedSizeInt(u64);

impl FixedSizeInt {
    pub const ENCODED_SIZE: usize = 8;
}

impl Encodable for FixedSizeInt {
    fn encode<S: Encoder>(&self, s: &mut S) -> Result<(), S::Error> {
        ((self.0 >>  0) as u8).encode(s)?;
        ((self.0 >>  8) as u8).encode(s)?;
        ((self.0 >> 16) as u8).encode(s)?;
        ((self.0 >> 24) as u8).encode(s)?;
        ((self.0 >> 32) as u8).encode(s)?;
        ((self.0 >> 40) as u8).encode(s)?;
        ((self.0 >> 48) as u8).encode(s)?;
        ((self.0 >> 56) as u8).encode(s)?;

        Ok(())
    }
}

impl Decodable for FixedSizeInt {
    fn decode<D: Decoder>(d: &mut D) -> Result<FixedSizeInt, D::Error> {
        let _0: u8 = Decodable::decode(d)?;
        let _1: u8 = Decodable::decode(d)?;
        let _2: u8 = Decodable::decode(d)?;
        let _3: u8 = Decodable::decode(d)?;
        let _4: u8 = Decodable::decode(d)?;
        let _5: u8 = Decodable::decode(d)?;
        let _6: u8 = Decodable::decode(d)?;
        let _7: u8 = Decodable::decode(d)?;

        Ok(FixedSizeInt(
            ((_0 as u64) <<  0) |
            ((_1 as u64) <<  8) |
            ((_2 as u64) << 16) |
            ((_3 as u64) << 24) |
            ((_4 as u64) << 32) |
            ((_5 as u64) << 40) |
            ((_6 as u64) << 48) |
            ((_7 as u64) << 56)
        ))
    }
}

impl<'sess> OnDiskCache<'sess> {
    /// Create a new OnDiskCache instance from the serialized data in `data`.
    /// Note that the current implementation (which only deals with diagnostics
    /// so far) will eagerly deserialize the complete cache. Once we are
    /// dealing with larger amounts of data (i.e. cached query results),
    /// deserialization will need to happen lazily.
    pub fn new(sess: &'sess Session, data: Vec<u8>, start_pos: usize) -> OnDiskCache<'sess> {
        debug_assert!(sess.opts.incremental.is_some());

        let (header, prev_diagnostics, prev_def_path_tables, query_result_index) = {
            use ty::codec::TyDecoder;

            let mut decoder = opaque::Decoder::new(&data[..], start_pos);
            let header = Header::decode(&mut decoder).unwrap();

            let mut decoder = CacheDecoder {
                tcx: None,
                opaque: decoder,
                codemap: sess.codemap(),
                prev_filemap_starts: &header.prev_filemap_starts,
                cnum_map: &IndexVec::new(),
                prev_def_path_tables: &Vec::new(),
            };

            let prev_diagnostics: FxHashMap<_, _> = {
                let diagnostics: EncodedPrevDiagnostics =
                    decode_tagged(&mut decoder, PREV_DIAGNOSTICS_TAG).unwrap();
                diagnostics.into_iter().collect()
            };

            let prev_def_path_tables: Vec<DefPathTable> =
                decode_tagged(&mut decoder, DEF_PATH_TABLE_TAG).unwrap();

            let query_result_index_pos = {
                let pos_pos = data.len() - FixedSizeInt::ENCODED_SIZE;
                decoder.with_position(pos_pos, |decoder| {
                    FixedSizeInt::decode(decoder)
                }).unwrap().0 as usize
            };

            let query_result_index: Vec<(SerializedDepNodeIndex, usize)> =
                decoder.with_position(query_result_index_pos, |decoder| {
                    decode_tagged(decoder, QUERY_RESULT_INDEX_TAG)
                }).unwrap();

            (header.clone(), prev_diagnostics, prev_def_path_tables, query_result_index)
        };

        OnDiskCache {
            serialized_data: data,
            prev_diagnostics,
            prev_filemap_starts: header.prev_filemap_starts,
            codemap: sess.codemap(),
            current_diagnostics: RefCell::new(FxHashMap()),
            prev_cnums: header.prev_cnums,
            cnum_map: RefCell::new(None),
            prev_def_path_tables,
            query_result_index: query_result_index.into_iter().collect(),
        }
    }

    pub fn new_empty(codemap: &'sess CodeMap) -> OnDiskCache<'sess> {
        OnDiskCache {
            serialized_data: vec![],
            prev_diagnostics: FxHashMap(),
            prev_filemap_starts: BTreeMap::new(),
            codemap,
            current_diagnostics: RefCell::new(FxHashMap()),
            prev_cnums: vec![],
            cnum_map: RefCell::new(None),
            prev_def_path_tables: Vec::new(),
            query_result_index: FxHashMap(),
        }
    }

    pub fn serialize<'a, 'gcx, 'lcx, E>(&self,
                                        tcx: TyCtxt<'a, 'gcx, 'lcx>,
                                        encoder: &mut E,
                                        cstore: &CrateStore)
                                        -> Result<(), E::Error>
        where E: ty_codec::TyEncoder
    {
        use ty::codec::TyEncoder;

        let _in_ignore = tcx.dep_graph.in_ignore();

        let mut encoder = CacheEncoder {
            encoder,
            type_shorthands: FxHashMap(),
            predicate_shorthands: FxHashMap(),
            definitions: tcx.hir.definitions(),
        };

        let prev_filemap_starts: BTreeMap<_, _> = self
            .codemap
            .files()
            .iter()
            .map(|fm| (fm.start_pos, StableFilemapId::new(fm)))
            .collect();

        let prev_cnums = tcx.all_crate_nums(LOCAL_CRATE).iter().map(|&cnum| {
            let crate_name = tcx.original_crate_name(cnum).as_str().to_string();
            let crate_disambiguator = tcx.crate_disambiguator(cnum);
            (cnum.as_u32(), crate_name, crate_disambiguator)
        }).collect();

        // for prev_cnum in &prev_cnums {
            // println!("encoded prev_cnum: {:?}", prev_cnum);
        // }

        // encoder.encode_tagged(HEADER_TAG, &Header {
        //     prev_filemap_starts ,
        //     prev_cnums,
        // })?;
        Header {
            prev_filemap_starts,
            prev_cnums,
        }.encode(&mut encoder)?;

        let diagnostics: EncodedPrevDiagnostics =
            self.current_diagnostics
                .borrow()
                .iter()
                .map(|(k, v)| (SerializedDepNodeIndex::new(k.index()), v.clone()))
                .collect();

        encoder.encode_tagged(PREV_DIAGNOSTICS_TAG, &diagnostics)?;

        // encoder.encode_tagged(DEF_PATH_TABLE_TAG, tcx.hir.definitions().def_path_table())?;


        let all_crate_nums = {
            let mut x: Vec<_> = (*tcx.all_crate_nums(LOCAL_CRATE)).clone();
            x.sort();
            x
        };

        let mut def_path_tables_rc = vec![None];
        for &cnum in all_crate_nums.iter() {
            assert!(cnum.index() == def_path_tables_rc.len());
            def_path_tables_rc.push(Some(cstore.def_path_table(cnum)));
        }

        let mut def_path_tables = vec![ tcx.hir.definitions().def_path_table()];
        def_path_tables.extend(def_path_tables_rc.iter().skip(1).map(|x| &**(x.as_ref().unwrap())));
        encoder.encode_tagged(DEF_PATH_TABLE_TAG, &def_path_tables)?;

        let mut query_result_index = Vec::with_capacity(
            tcx.maps.optimized_mir.borrow().map.len() +
            tcx.maps.typeck_tables_of.borrow().map.len()
        );

        for (def_id, entry) in tcx.maps.optimized_mir.borrow().map.iter() {

            if !def_id.is_local() {
                continue
            }

            // println!("storing {:?}:\n{:?}", def_id, entry.value);

            let index = SerializedDepNodeIndex::new(entry.index.index());
            query_result_index.push((index, encoder.position()));

            let mir: &::mir::Mir<'gcx> = &entry.value;
            encoder.encode_tagged(index, mir)?;

            let fingerprint = tcx.dep_graph.fingerprint_of_index(entry.index);
            encoder.encode_tagged(QUERY_RESULT_ICH_TAG, &fingerprint)?;
        }

        for (def_id, entry) in tcx.maps.typeck_tables_of.borrow().map.iter() {
            if !def_id.is_local() {
                continue
            }

            let index = SerializedDepNodeIndex::new(entry.index.index());
            query_result_index.push((index, encoder.position()));

            let typeck_tables: &ty::TypeckTables<'gcx> = &entry.value;

            encoder.encode_tagged(index, typeck_tables)?;

            let fingerprint = tcx.dep_graph.fingerprint_of_index(entry.index);
            encoder.encode_tagged(QUERY_RESULT_ICH_TAG, &fingerprint)?;
        }

        let query_result_index_pos: u64 = (encoder.position()) as u64;

        encoder.encode_tagged(QUERY_RESULT_INDEX_TAG, &query_result_index)?;

        FixedSizeInt(query_result_index_pos).encode(&mut encoder)?;

        Ok(())
    }

    /// Load a diagnostic emitted during the previous compilation session.
    pub fn load_diagnostics(&self,
                            dep_node_index: SerializedDepNodeIndex)
                            -> Vec<Diagnostic> {
        self.prev_diagnostics.get(&dep_node_index).cloned().unwrap_or(vec![])
    }

    /// Store a diagnostic emitted during the current compilation session.
    /// Anything stored like this will be available via `load_diagnostics` in
    /// the next compilation session.
    pub fn store_diagnostics(&self,
                             dep_node_index: DepNodeIndex,
                             diagnostics: Vec<Diagnostic>) {
        let mut current_diagnostics = self.current_diagnostics.borrow_mut();
        let prev = current_diagnostics.insert(dep_node_index, diagnostics);
        debug_assert!(prev.is_none());
    }

    /// Store a diagnostic emitted during computation of an anonymous query.
    /// Since many anonymous queries can share the same `DepNode`, we aggregate
    /// them -- as opposed to regular queries where we assume that there is a
    /// 1:1 relationship between query-key and `DepNode`.
    pub fn store_diagnostics_for_anon_node(&self,
                                           dep_node_index: DepNodeIndex,
                                           mut diagnostics: Vec<Diagnostic>) {
        let mut current_diagnostics = self.current_diagnostics.borrow_mut();

        let x = current_diagnostics.entry(dep_node_index).or_insert_with(|| {
            mem::replace(&mut diagnostics, Vec::new())
        });

        x.extend(diagnostics.into_iter());
    }

    pub fn load_query_result<'a, 'tcx, T>(&self,
                                          tcx: TyCtxt<'a, 'tcx, 'tcx>,
                                          dep_node_index: SerializedDepNodeIndex)
                                          -> T
        where T: ::std::fmt::Debug + Decodable + ::rustc_data_structures::stable_hasher::HashStable< ::ich::StableHashingContext<'tcx> >
    {
        // let dep_node = tcx.dep_graph.prev_dep_node_index_to_dep_node(dep_node_index);

        // println!("Loading query result for {:?}", dep_node);

        let pos = self.query_result_index[&dep_node_index];

        let mut cnum_map = self.cnum_map.borrow_mut();
        if cnum_map.is_none() {
            *cnum_map = Some(Self::compute_cnum_map(tcx, &self.prev_cnums[..]));
        }

        let mut decoder = CacheDecoder {
            tcx: Some(tcx),
            opaque: opaque::Decoder::new(&self.serialized_data[..], pos),
            codemap: self.codemap,
            prev_filemap_starts: &self.prev_filemap_starts,
            cnum_map: cnum_map.as_ref().unwrap(),
            prev_def_path_tables: &self.prev_def_path_tables,
        };

        let value: T = decode_tagged(&mut decoder, dep_node_index).unwrap();
        let _cached_fingerprint: ::ich::Fingerprint = decode_tagged(&mut decoder, QUERY_RESULT_ICH_TAG).unwrap();

        // {
        //     use rustc_data_structures::stable_hasher::{StableHasher};

        //     let mut hcx = tcx.create_stable_hashing_context();
        //     let mut hasher = StableHasher::new();

        //     value.hash_stable(&mut hcx, &mut hasher);

        //     let new_fingerprint = hasher.finish();

        //     if cached_fingerprint != new_fingerprint {
        //         println!("MISMATCH VALUE:\n{:?}", value);
        //     }

        //     assert!(cached_fingerprint == new_fingerprint, "Fingerprint mismatch for {:?}", dep_node);
        // }

        value

        // match decode_tagged(&mut decoder, dep_node_index) {
        //     Ok(value) => {
        //         value
        //     }
        //     Err(e) => {
        //         bug!("Could not decode cached query result: {}", e)
        //     }
        // }
    }

    fn compute_cnum_map(tcx: TyCtxt,
                        keys: &[(u32, String, CrateDisambiguator)])
                        -> IndexVec<CrateNum, Option<CrateNum>>
    {
        let _in_ignore = tcx.dep_graph.in_ignore();

        // for key in keys {
        //     println!("loaded prev_cnum: {:?}", key);
        // }

        let current = tcx.all_crate_nums(LOCAL_CRATE).iter().map(|&cnum| {
            let crate_name = tcx.original_crate_name(cnum)
                                .as_str()
                                .to_string();
            let crate_disambiguator = tcx.crate_disambiguator(cnum);

            // println!("current cnum: {:?}", (cnum.as_u32(), &crate_name, crate_disambiguator));

            ((crate_name, crate_disambiguator), cnum)
        }).collect::<FxHashMap<_,_>>();

        let map_size = keys.iter().map(|&(cnum, ..)| cnum).max().unwrap() + 1;
        let mut map = IndexVec::with_capacity(map_size as usize);
        for _ in 0 .. map_size {
            map.push(None);
        }

        for &(prev_cnum, ref crate_name, crate_disambiguator) in keys {
            let key = (crate_name.clone(), crate_disambiguator);
            map[CrateNum::from_u32(prev_cnum)] = current.get(&key).cloned();
        }

        map[LOCAL_CRATE] = Some(LOCAL_CRATE);

        map
    }
}

/// A decoder that can read the incr. comp. cache. It is similar to the one
/// we use for crate metadata decoding in that it can rebase spans and
/// eventually will also handle things that contain `Ty` instances.
struct CacheDecoder<'a, 'tcx: 'a, 'x> {
    tcx: Option<TyCtxt<'a, 'tcx, 'tcx>>,
    opaque: opaque::Decoder<'x>,
    codemap: &'x CodeMap,
    prev_filemap_starts: &'x BTreeMap<BytePos, StableFilemapId>,
    cnum_map: &'x IndexVec<CrateNum, Option<CrateNum>>,
    prev_def_path_tables: &'x Vec<DefPathTable>,
}

impl<'a, 'tcx, 'x> CacheDecoder<'a, 'tcx, 'x> {
    fn find_filemap_prev_bytepos(&self,
                                 prev_bytepos: BytePos)
                                 -> Option<(BytePos, StableFilemapId)> {
        for (start, id) in self.prev_filemap_starts.range(BytePos(0) ..= prev_bytepos).rev() {
            return Some((*start, *id))
        }

        None
    }
}

fn decode_tagged<'a, 'tcx, D, T, V>(decoder: &mut D, expected_tag: T) -> Result<V, D::Error>
    where T: Decodable + Eq + ::std::fmt::Debug,
          V: Decodable,
          D: Decoder + ty_codec::TyDecoder<'a, 'tcx>,
          'tcx: 'a,
{
    let start_pos = decoder.position();

    let actual_tag = T::decode(decoder)?;
    assert_eq!(actual_tag, expected_tag);
    let value = V::decode(decoder)?;
    let end_pos = decoder.position();

    let expected_len: u64 = Decodable::decode(decoder)?;


    assert_eq!((end_pos - start_pos) as u64, expected_len);

    Ok(value)
}

macro_rules! decoder_methods {
    ($($name:ident -> $ty:ty;)*) => {
        $(fn $name(&mut self) -> Result<$ty, Self::Error> {
            self.opaque.$name()
        })*
    }
}

impl<'a, 'tcx, 'x> Decoder for CacheDecoder<'a, 'tcx, 'x> {
    type Error = String;

    decoder_methods! {
        read_nil -> ();

        read_u128 -> u128;
        read_u64 -> u64;
        read_u32 -> u32;
        read_u16 -> u16;
        read_u8 -> u8;
        read_usize -> usize;

        read_i128 -> i128;
        read_i64 -> i64;
        read_i32 -> i32;
        read_i16 -> i16;
        read_i8 -> i8;
        read_isize -> isize;

        read_bool -> bool;
        read_f64 -> f64;
        read_f32 -> f32;
        read_char -> char;
        read_str -> Cow<str>;
    }

    fn error(&mut self, err: &str) -> Self::Error {
        self.opaque.error(err)
    }
}

impl<'a, 'tcx, 'x> SpecializedDecoder<Span> for CacheDecoder<'a, 'tcx, 'x> {
    fn specialized_decode(&mut self) -> Result<Span, Self::Error> {
        let lo = BytePos::decode(self)?;
        let hi = BytePos::decode(self)?;

        if let Some((prev_filemap_start, filemap_id)) = self.find_filemap_prev_bytepos(lo) {
            if let Some(current_filemap) = self.codemap.filemap_by_stable_id(filemap_id) {
                let lo = (lo + current_filemap.start_pos) - prev_filemap_start;
                let hi = (hi + current_filemap.start_pos) - prev_filemap_start;
                return Ok(Span::new(lo, hi, NO_EXPANSION));
            }
        }

        Ok(DUMMY_SP)
    }
}

impl<'a, 'tcx: 'a, 'x> ty_codec::TyDecoder<'a, 'tcx> for CacheDecoder<'a, 'tcx, 'x> {

    fn tcx(&self) -> TyCtxt<'a, 'tcx, 'tcx> {
        self.tcx.expect("missing TyCtxt in CacheDecoder")
    }

    fn position(&self) -> usize {
        self.opaque.position()
    }

    fn peek_byte(&self) -> u8 {
        self.opaque.data[self.opaque.position()]
    }

    fn cached_ty_for_shorthand<F>(&mut self,
                                  shorthand: usize,
                                  or_insert_with: F)
                                  -> Result<Ty<'tcx>, Self::Error>
        where F: FnOnce(&mut Self) -> Result<Ty<'tcx>, Self::Error>
    {
        let tcx = self.tcx();

        let cache_key = ty::CReaderCacheKey {
            cnum: INCR_CACHE_CRATE,
            pos: shorthand,
        };

        if let Some(&ty) = tcx.rcache.borrow().get(&cache_key) {
            return Ok(ty);
        }

        let ty = or_insert_with(self)?;
        tcx.rcache.borrow_mut().insert(cache_key, ty);
        Ok(ty)
    }

    fn with_position<F, R>(&mut self, pos: usize, f: F) -> R
        where F: FnOnce(&mut Self) -> R
    {
        debug_assert!(pos < self.opaque.data.len());

        let new_opaque = opaque::Decoder::new(self.opaque.data, pos);
        let old_opaque = mem::replace(&mut self.opaque, new_opaque);
        let r = f(self);
        self.opaque = old_opaque;
        r
    }

    fn map_encoded_cnum_to_current(&self, cnum: CrateNum) -> CrateNum {
        self.cnum_map[cnum].unwrap_or_else(|| {
            bug!("Could not find new CrateNum for {:?}", cnum)
        })
    }
}

impl<'a, 'tcx, 'x> SpecializedDecoder<CrateNum> for CacheDecoder<'a, 'tcx, 'x> {
    fn specialized_decode(&mut self) -> Result<CrateNum, Self::Error> {
        use ty::codec::TyDecoder;
        let cnum = CrateNum::from_u32(u32::decode(self)?);
        let mapped = self.map_encoded_cnum_to_current(cnum);

        // println!("mapping cnum {} to {}", cnum.as_u32(), mapped.as_u32());
        Ok(mapped)
    }
}

// Todo: Assert that DefIndex is not vanilla decoded.
impl<'a, 'tcx, 'x> SpecializedDecoder<DefIndex> for CacheDecoder<'a, 'tcx, 'x> {
    fn specialized_decode(&mut self) -> Result<DefIndex, Self::Error> {
        panic!("dont use this")
        // use ty::codec::TyDecoder;
        // use serialize::UseSpecializedDecodable;
        // let tcx = self.tcx();

        // let def_index = DefIndex::default_decode(self)?;
        // let def_path_hash = self.prev_def_path_tables[LOCAL_CRATE.index()].def_path_hash(def_index);
        // let def_id = tcx.def_path_hash_to_def_id.as_ref().unwrap()[&def_path_hash];

        // println!("decoding DefIndex: {:?} -> {:?}", def_index, def_id.index);

        // Ok(def_id.index)
    }
}

impl<'a, 'tcx, 'x> SpecializedDecoder<DefId> for CacheDecoder<'a, 'tcx, 'x> {
    fn specialized_decode(&mut self) -> Result<DefId, Self::Error> {
        use ty::codec::TyDecoder;
        use serialize::UseSpecializedDecodable;

        let prev_cnum = CrateNum::default_decode(self)?;
        let tcx = self.tcx();

        let def_index = DefIndex::default_decode(self)?;
        let def_path_hash = self.prev_def_path_tables[prev_cnum.index()]
                                .def_path_hash(def_index);

        let def_id = tcx.def_path_hash_to_def_id.as_ref().unwrap()[&def_path_hash];

        // println!("decoding DefId: {:?} -> {:?}", def_index, def_id.index);

        Ok(def_id)
    }
}

impl<'a, 'tcx, 'x> SpecializedDecoder<NodeId> for CacheDecoder<'a, 'tcx, 'x> {
    fn specialized_decode(&mut self) -> Result<NodeId, Self::Error> {
        use ty::codec::TyDecoder;
        use serialize::UseSpecializedDecodable;

        // println!("decoding NodeId");
        // let def_index = DefIndex::decode(self)?;
        let def_index = DefIndex::default_decode(self)?;

        let def_path_hash = self.prev_def_path_tables[LOCAL_CRATE.index()]
                                .def_path_hash(def_index);
        let def_id = self.tcx().def_path_hash_to_def_id.as_ref().unwrap()[&def_path_hash];

        let local_id = hir::ItemLocalId::decode(self)?;

        Ok(self.tcx().hir.hir_to_node_id(hir::HirId {
            owner: def_id.index,
            local_id
        }))
    }
}

impl<'enc, 'tcx, E> SpecializedEncoder<NodeId> for CacheEncoder<'enc, 'tcx, E>
    where E: 'enc + ty_codec::TyEncoder
{
    fn specialized_encode(&mut self, node_id: &NodeId) -> Result<(), Self::Error> {
        let hir_id = self.definitions.node_to_hir_id(*node_id);
        hir_id.owner.encode(self)?;
        hir_id.local_id.encode(self)
    }
}

// impl serialize::UseSpecializedEncodable for NodeId {
//     fn default_encode<S: Encoder>(&self, s: &mut S) -> Result<(), S::Error> {
//         s.emit_u32(self.0)
//     }
// }

// impl serialize::UseSpecializedDecodable for NodeId {
//     fn default_decode<D: Decoder>(d: &mut D) -> Result<NodeId, D::Error> {
//         d.read_u32().map(NodeId)
//     }
// }


impl<'a, 'tcx, 'x> SpecializedDecoder<Ty<'tcx>> for CacheDecoder<'a, 'tcx, 'x> {
    fn specialized_decode(&mut self) -> Result<Ty<'tcx>, Self::Error> {
        ty_codec::decode_ty(self)
    }
}

impl<'a, 'tcx, 'x> SpecializedDecoder<ty::GenericPredicates<'tcx>> for CacheDecoder<'a, 'tcx, 'x> {
    fn specialized_decode(&mut self) -> Result<ty::GenericPredicates<'tcx>, Self::Error> {
        ty_codec::decode_predicates(self)
    }
}

impl<'a, 'tcx, 'x> SpecializedDecoder<&'tcx Substs<'tcx>> for CacheDecoder<'a, 'tcx, 'x> {
    fn specialized_decode(&mut self) -> Result<&'tcx Substs<'tcx>, Self::Error> {
        ty_codec::decode_substs(self)
    }
}

impl<'a, 'tcx, 'x> SpecializedDecoder<ty::Region<'tcx>> for CacheDecoder<'a, 'tcx, 'x> {
    fn specialized_decode(&mut self) -> Result<ty::Region<'tcx>, Self::Error> {
        ty_codec::decode_region(self)
    }
}

impl<'a, 'tcx, 'x> SpecializedDecoder<&'tcx ty::Slice<Ty<'tcx>>> for CacheDecoder<'a, 'tcx, 'x> {
    fn specialized_decode(&mut self) -> Result<&'tcx ty::Slice<Ty<'tcx>>, Self::Error> {
        ty_codec::decode_ty_slice(self)
    }
}

impl<'a, 'tcx, 'x> SpecializedDecoder<&'tcx ty::AdtDef> for CacheDecoder<'a, 'tcx, 'x> {
    fn specialized_decode(&mut self) -> Result<&'tcx ty::AdtDef, Self::Error> {
        ty_codec::decode_adt_def(self)
    }
}

impl<'a, 'tcx, 'x> SpecializedDecoder<&'tcx ty::Slice<ty::ExistentialPredicate<'tcx>>>
    for CacheDecoder<'a, 'tcx, 'x> {
    fn specialized_decode(&mut self)
        -> Result<&'tcx ty::Slice<ty::ExistentialPredicate<'tcx>>, Self::Error> {
        ty_codec::decode_existential_predicate_slice(self)
    }
}

impl<'a, 'tcx, 'x> SpecializedDecoder<ByteArray<'tcx>> for CacheDecoder<'a, 'tcx, 'x> {
    fn specialized_decode(&mut self) -> Result<ByteArray<'tcx>, Self::Error> {
        ty_codec::decode_byte_array(self)
    }
}

impl<'a, 'tcx, 'x> SpecializedDecoder<&'tcx ty::Const<'tcx>> for CacheDecoder<'a, 'tcx, 'x> {
    fn specialized_decode(&mut self) -> Result<&'tcx ty::Const<'tcx>, Self::Error> {
        ty_codec::decode_const(self)
    }
}

impl<'a, 'tcx, 'x> SpecializedDecoder<&'tcx Mir<'tcx>> for CacheDecoder<'a, 'tcx, 'x> {
    fn specialized_decode(&mut self) -> Result<&'tcx Mir<'tcx>, Self::Error> {
        ty_codec::decode_mir(self)
    }
}

// impl<'a, 'tcx, 'x> SpecializedDecoder<&'tcx ty::TypeckTables<'tcx>> for CacheDecoder<'a, 'tcx, 'x> {
//     fn specialized_decode(&mut self) -> Result<&'tcx ty::TypeckTables<'tcx>, Self::Error> {
//         Ok(self.tcx.unwrap().alloc_tables(ty::TypeckTables::decode(self)?))
//     }
// }

struct CacheEncoder<'enc, 'tcx, E>
    where E: 'enc + ty_codec::TyEncoder
{
    encoder: &'enc mut E,
    type_shorthands: FxHashMap<Ty<'tcx>, usize>,
    predicate_shorthands: FxHashMap<ty::Predicate<'tcx>, usize>,
    definitions: &'enc Definitions,
}

impl<'enc, 'tcx, E> CacheEncoder<'enc, 'tcx, E>
    where E: 'enc + ty_codec::TyEncoder
{
    fn encode_tagged<T: Encodable, V: Encodable>(&mut self,
                                                 tag: T,
                                                 value: &V)
                                                 -> Result<(), E::Error>
    {
        use ty::codec::TyEncoder;

        let start_pos = self.position();

        tag.encode(self)?;
        value.encode(self)?;

        let end_pos = self.position();

        ((end_pos - start_pos) as u64).encode(self)
    }
}


impl<'enc, 'tcx, E> ty_codec::TyEncoder for CacheEncoder<'enc, 'tcx, E>
    where E: 'enc + ty_codec::TyEncoder
{
    fn position(&self) -> usize {
        self.encoder.position()
    }
}

impl<'enc, 'tcx, E> SpecializedEncoder<Ty<'tcx>> for CacheEncoder<'enc, 'tcx, E>
    where E: 'enc + ty_codec::TyEncoder
{
    fn specialized_encode(&mut self, ty: &Ty<'tcx>) -> Result<(), Self::Error> {
        ty_codec::encode_with_shorthand(self, ty,
            |encoder| &mut encoder.type_shorthands)
    }
}

impl<'enc, 'tcx, E> SpecializedEncoder<ty::GenericPredicates<'tcx>>
    for CacheEncoder<'enc, 'tcx, E>
    where E: 'enc + ty_codec::TyEncoder
{
    fn specialized_encode(&mut self,
                          predicates: &ty::GenericPredicates<'tcx>)
                          -> Result<(), Self::Error> {
        ty_codec::encode_predicates(self, predicates,
            |encoder| &mut encoder.predicate_shorthands)
    }
}

macro_rules! encoder_methods {
    ($($name:ident($ty:ty);)*) => {
        $(fn $name(&mut self, value: $ty) -> Result<(), Self::Error> {
            self.encoder.$name(value)
        })*
    }
}

impl<'enc, 'tcx, E> Encoder for CacheEncoder<'enc, 'tcx, E>
    where E: 'enc + ty_codec::TyEncoder
{
    type Error = E::Error;

    fn emit_nil(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }

    encoder_methods! {
        emit_usize(usize);
        emit_u128(u128);
        emit_u64(u64);
        emit_u32(u32);
        emit_u16(u16);
        emit_u8(u8);

        emit_isize(isize);
        emit_i128(i128);
        emit_i64(i64);
        emit_i32(i32);
        emit_i16(i16);
        emit_i8(i8);

        emit_bool(bool);
        emit_f64(f64);
        emit_f32(f32);
        emit_char(char);
        emit_str(&str);
    }
}
