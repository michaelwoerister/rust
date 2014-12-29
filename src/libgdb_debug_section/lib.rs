
// #![feature(linkage)]
#![crate_name = "gdb_debug_section"]
#![crate_type = "rlib"]

#[link_section = ".debug_gdb_scripts"]
// #[linkage = "linkonce_odr"]
pub static __RUSTC_DEBUG_GDB_SCRIPTS_SECTION__: [u8, ..34] =
   [1, // this means that the scripting language is Python
    b'g',
    b'd',
    b'b',
    b'_',
    b'l',
    b'o',
    b'a',
    b'd',
    b'_',
    b'r',
    b'u',
    b's',
    b't',
    b'_',
    b'p',
    b'r',
    b'e',
    b't',
    b't',
    b'y',
    b'_',
    b'p',
    b'r',
    b'i',
    b'n',
    b't',
    b'e',
    b'r',
    b's',
    b'.',
    b'p',
    b'y',
    0];
