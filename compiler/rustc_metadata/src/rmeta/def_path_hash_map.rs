use rustc_span::def_id::{DefIndex, DefPathHash};
use rustc_data_structures::fingerprint::Fingerprint;
use std::convert::TryInto;
use rustc_serialize::{opaque, Encodable, Decodable, Encoder, Decoder};
use crate::rmeta::EncodeContext;
use crate::rmeta::DecodeContext;

crate struct HashMapConfig;

impl odht::Config for HashMapConfig {
    type Key = DefPathHash;
    type Value = DefIndex;

    type RawKey = [u8; 16];
    type RawValue = [u8; 4];

    type H = odht::UnHashFn;

    #[inline]
    fn encode_key(k: &DefPathHash) -> [u8; 16] {
        // This seems to optimize to the same machine code as
        // `unsafe { mem::transmute(*k) }`. Well done, LLVM! :)

        let mut result = [0u8; 16];

        let first_half: &mut [u8; 8] = (&mut result[0 .. 8]).try_into().unwrap();
        *first_half = k.0.as_value().0.to_le_bytes();

        let second_half: &mut [u8; 8] = (&mut result[8 .. 16]).try_into().unwrap();
        *second_half = k.0.as_value().1.to_le_bytes();

        result
    }

    #[inline]
    fn encode_value(v: &DefIndex) -> [u8; 4] {
        v.as_u32().to_le_bytes()
    }

    #[inline]
    fn decode_key(k: &[u8; 16]) -> DefPathHash {
        let fingerprint = Fingerprint::new(
            u64::from_le_bytes(k[0 .. 8].try_into().unwrap()),
            u64::from_le_bytes(k[8 .. 16].try_into().unwrap()),
        );

        DefPathHash(fingerprint)
    }

    #[inline]
    fn decode_value(v: &[u8; 4]) -> DefIndex {
        DefIndex::from_u32(u32::from_le_bytes(*v))
    }
}

crate struct DefPathHashMap(odht::HashTableBuilder<HashMapConfig>);

impl DefPathHashMap {
    pub fn build(def_path_hashes: impl Iterator<Item = (DefPathHash, DefIndex)>) -> DefPathHashMap {
        let builder = odht::HashTableBuilder::<HashMapConfig>::from_iterator(def_path_hashes, 0.85);
        DefPathHashMap(builder)
    }

    #[inline]
    pub fn def_path_hash_to_def_index(&self, def_path_hash: &DefPathHash) -> Option<DefIndex> {
        self.0.get(def_path_hash)
    }
}


impl<'a, 'tcx> Encodable<EncodeContext<'a, 'tcx>> for DefPathHashMap {
    fn encode(&self, e: &mut EncodeContext<'a, 'tcx>) -> opaque::EncodeResult {
        // FIXME: Provide a `HashTableBuilder` method that does this.
        let bytes_needed = (self.0.len() as f32 * 1.3) as usize * (4 + 16) + 64;
        let mut cursor = std::io::Cursor::new(Vec::with_capacity(bytes_needed));
        self.0.serialize(&mut cursor).unwrap();
        let bytes = cursor.into_inner();

        e.emit_usize(bytes.len()).unwrap();
        e.emit_raw_bytes(&bytes[..]);

        Ok(())
    }
}

impl<'a, 'tcx> Decodable<DecodeContext<'a, 'tcx>> for DefPathHashMap {
    fn decode(d: &mut DecodeContext<'a, 'tcx>) -> Result<DefPathHashMap, String> {
        let len = d.read_usize()?;
        let bytes = d.read_non_copy(len);

        let inner = odht::HashTableBuilder::<HashMapConfig>::from_serialized(&bytes[..])
            .map_err(|e| format!("{}", e))?;

        Ok(DefPathHashMap(inner))
    }
}
