use crate::rmeta::DecodeContext;
use crate::rmeta::EncodeContext;
use rustc_data_structures::fingerprint::Fingerprint;
use rustc_serialize::{opaque, Decodable, Decoder, Encodable, Encoder};
use rustc_span::def_id::{DefIndex, DefPathHash};

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

crate struct DefPathHashMap(odht::HashTableOwned<HashMapConfig>);

impl DefPathHashMap {
    pub fn build(def_path_hashes: impl Iterator<Item = (DefPathHash, DefIndex)>) -> DefPathHashMap {
        let builder = odht::HashTableOwned::<HashMapConfig>::from_iterator(def_path_hashes, 85);
        DefPathHashMap(builder)
    }

    #[inline]
    pub fn def_path_hash_to_def_index(&self, def_path_hash: &DefPathHash) -> Option<DefIndex> {
        self.0.get(def_path_hash)
    }
}

impl<'a, 'tcx> Encodable<EncodeContext<'a, 'tcx>> for DefPathHashMap {
    fn encode(&self, e: &mut EncodeContext<'a, 'tcx>) -> opaque::EncodeResult {
        let bytes = self.0.raw_bytes();

        e.emit_usize(bytes.len())?;
        e.emit_raw_bytes(&bytes[..]);

        Ok(())
    }
}

impl<'a, 'tcx> Decodable<DecodeContext<'a, 'tcx>> for DefPathHashMap {
    fn decode(d: &mut DecodeContext<'a, 'tcx>) -> Result<DefPathHashMap, String> {
        let len = d.read_usize()?;
        let bytes = d.read_raw_bytes(len);

        let inner = odht::HashTableOwned::<HashMapConfig>::from_raw_bytes(&bytes[..])
            .map_err(|e| format!("{}", e))?;

        Ok(DefPathHashMap(inner))
    }
}
