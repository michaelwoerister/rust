
// fn main() {
//     let is_zero = &|&&d: &&u8| -> bool { d == b'0' };
//     &[0u8].iter().take_while(is_zero).count();
// }


// fn take_fn_once<F: FnOnce()>(f: F) {
//     (f)();
// }

// fn take_fn<F: Fn()>(f: F) {
//     take_fn_once(f);
// }

// fn main() {
//     let f = || {
//         ()
//     };

//     take_fn(f);
// }

#![feature(rustc_attrs)]

#[rustc_mir(graphviz="match.dot")]
fn main() {
    let _abc = match Some(101i8) {
        Some(xyz) if xyz > 100 => xyz,
        Some(_) => -1,
        None => -2
    };
}
