

mod def_path_hash;
mod hashing_context;
mod fingerprint;
mod hir_hasher;

pub use self::def_path_hash::DefPathHashes;
pub use self::hashing_context::StableHashingContext;
pub use self::fingerprint::Fingerprint;
pub use self::hir_hasher::StrictVersionHashVisitor;

pub const ATTR_DIRTY: &'static str = "rustc_dirty";
pub const ATTR_CLEAN: &'static str = "rustc_clean";
pub const ATTR_DIRTY_METADATA: &'static str = "rustc_metadata_dirty";
pub const ATTR_CLEAN_METADATA: &'static str = "rustc_metadata_clean";
pub const ATTR_IF_THIS_CHANGED: &'static str = "rustc_if_this_changed";
pub const ATTR_THEN_THIS_WOULD_NEED: &'static str = "rustc_then_this_would_need";

pub const IGNORED_ATTRIBUTES: &'static [&'static str] = &[
    "cfg",
    ATTR_IF_THIS_CHANGED,
    ATTR_THEN_THIS_WOULD_NEED,
    ATTR_DIRTY,
    ATTR_CLEAN,
    ATTR_DIRTY_METADATA,
    ATTR_CLEAN_METADATA
];

pub type IchHasher = ::rustc_data_structures::stable_hasher::StableHasher<Fingerprint>;
