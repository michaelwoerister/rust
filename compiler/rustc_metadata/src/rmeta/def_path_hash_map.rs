use crate::rmeta::DecodeContext;
use crate::rmeta::EncodeContext;
use rustc_data_structures::fingerprint::Fingerprint;
use rustc_serialize::{opaque, Decodable, Decoder, Encodable, Encoder};
use rustc_span::def_id::{DefIndex, DefPathHash};
use rustc_data_structures::owning_ref::OwningRef;
use crate::rmeta::MetadataBlob;

crate struct HashMapConfig;

impl odht::Config for HashMapConfig {
    type Key = DefPathHash;
    type Value = DefIndex;

    type EncodedKey = [u8; 16];
    type EncodedValue = [u8; 4];

    type H = odht::UnHashFn;

    #[inline]
    fn encode_key(k: &DefPathHash) -> [u8; 16] {
        k.0.to_le_bytes()
    }

    #[inline]
    fn encode_value(v: &DefIndex) -> [u8; 4] {
        v.as_u32().to_le_bytes()
    }

    #[inline]
    fn decode_key(k: &[u8; 16]) -> DefPathHash {
        DefPathHash(Fingerprint::from_le_bytes(*k))
    }

    #[inline]
    fn decode_value(v: &[u8; 4]) -> DefIndex {
        DefIndex::from_u32(u32::from_le_bytes(*v))
    }
}

crate enum DefPathHashMap {
    Owned(odht::HashTableOwned<HashMapConfig>),
    Borrowed(odht::HashTable<HashMapConfig, OwningRef<MetadataBlob, [u8]>>),
}

impl DefPathHashMap {
    pub fn build(def_path_hashes: impl Iterator<Item = (DefPathHash, DefIndex)>) -> DefPathHashMap {
        let builder = odht::HashTableOwned::<HashMapConfig>::from_iterator(def_path_hashes, 85);
        DefPathHashMap::Owned(builder)
    }

    #[inline]
    pub fn def_path_hash_to_def_index(&self, def_path_hash: &DefPathHash) -> Option<DefIndex> {
        match *self {
            DefPathHashMap::Owned(ref map) => map.get(def_path_hash),
            DefPathHashMap::Borrowed(ref map) => map.get(def_path_hash),
        }
    }
}

impl<'a, 'tcx> Encodable<EncodeContext<'a, 'tcx>> for DefPathHashMap {
    fn encode(&self, e: &mut EncodeContext<'a, 'tcx>) -> opaque::EncodeResult {
        let bytes = match *self {
            DefPathHashMap::Owned(ref map) => map.raw_bytes(),
            DefPathHashMap::Borrowed(_) => panic!(),
        };

        e.emit_usize(bytes.len())?;
        e.emit_raw_bytes(&bytes[..]);

        Ok(())
    }
}

impl<'a, 'tcx> Decodable<DecodeContext<'a, 'tcx>> for DefPathHashMap {
    fn decode(d: &mut DecodeContext<'a, 'tcx>) -> Result<DefPathHashMap, String> {
        let len = d.read_usize()?;
        // let bytes = d.read_raw_bytes(len);

        let pos = d.opaque.position();
        let blob = d.cdata().blob.clone();
        let o = OwningRef::new(blob);
        let o = o.map(|x| &x[pos .. pos + len]);

        // let inner = odht::HashTableOwned::<HashMapConfig>::from_raw_bytes(&bytes[..])
        //     .map_err(|e| format!("{}", e))?;
        let inner = odht::HashTable::from_raw_bytes(o).map_err(|e| format!("{}", e))?;

        Ok(DefPathHashMap::Borrowed(inner))
    }
}
