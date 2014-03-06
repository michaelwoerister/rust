// Copyright 2012 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use common::Config;
use procsrv;
use procsrv::{ProcRes, ProcArgs};

use std::io::File;

#[cfg(target_os = "win32")]
use std::os::getenv;

/// Conversion table from triple OS name to Rust SYSNAME
static OS_TABLE: &'static [(&'static str, &'static str)] = &[
    ("mingw32", "win32"),
    ("win32", "win32"),
    ("darwin", "macos"),
    ("android", "android"),
    ("linux", "linux"),
    ("freebsd", "freebsd"),
];

pub fn get_os(triple: &str) -> &'static str {
    for &(triple_os, os) in OS_TABLE.iter() {
        if triple.contains(triple_os) {
            return os
        }
    }
    fail!("Cannot determine OS from triple");
}

#[cfg(target_os = "win32")]
pub fn make_new_path(path: &str) -> ~str {

    // Windows just uses PATH as the library search path, so we have to
    // maintain the current value while adding our own
    match getenv(lib_path_env_var()) {
      Some(curr) => {
        format!("{}{}{}", path, path_div(), curr)
      }
      None => path.to_str()
    }
}

#[cfg(target_os = "win32")]
pub fn lib_path_env_var() -> ~str { ~"PATH" }

#[cfg(target_os = "win32")]
pub fn path_div() -> ~str { ~";" }

pub fn logv(config: &Config, s: ~str) {
    debug!("{}", s);
    if config.verbose { println!("{}", s); }
}

pub fn output_testname(testfile: &Path) -> Path {
    Path::new(testfile.filestem().unwrap())
}

pub fn output_base_name(config: &Config, testfile: &Path) -> Path {
    config.build_base
        .join(&output_testname(testfile))
        .with_extension(config.stage_id.as_slice())
}

pub fn dump_output_file(config: &Config, testfile: &Path,
                    out: &str, extension: &str) {
    let outfile = make_out_name(config, testfile, extension);
    File::create(&outfile).write(out.as_bytes()).unwrap();
}

pub fn make_out_name(config: &Config, testfile: &Path, extension: &str) -> Path {
    output_base_name(config, testfile).with_extension(extension)
}

// Linux and mac don't require adjusting the library search path
#[cfg(target_os = "linux")]
#[cfg(target_os = "macos")]
#[cfg(target_os = "freebsd")]
pub fn make_cmdline(_libpath: &str, prog: &str, args: &[~str]) -> ~str {
    format!("{} {}", prog, args.connect(" "))
}

#[cfg(target_os = "win32")]
pub fn make_cmdline(libpath: &str, prog: &str, args: &[~str]) -> ~str {
    format!("{} {} {}", lib_path_cmd_prefix(libpath), prog,
         args.connect(" "))
}

pub fn dump_output(config: &Config, testfile: &Path, out: &str, err: &str) {
    dump_output_file(config, testfile, out, "out");
    dump_output_file(config, testfile, err, "err");
    maybe_dump_to_stdout(config, out, err);
}

fn maybe_dump_to_stdout(config: &Config, out: &str, err: &str) {
    if config.verbose {
        println!("------{}------------------------------", "stdout");
        println!("{}", out);
        println!("------{}------------------------------", "stderr");
        println!("{}", err);
        println!("------------------------------------------");
    }
}

pub fn compose_and_run(config: &Config,
                   testfile: &Path,
                   ProcArgs { prog, args }: ProcArgs,
                   env: ~[(~str, ~str)],
                   lib_path: &str,
                   input: Option<~str>) -> ProcRes {
    let cmdline = {
        let cmdline = make_cmdline(lib_path, prog, args);
        logv(config, format!("executing {}", cmdline));
        cmdline
    };
    let procsrv::Result { out, err, status } = procsrv::run(lib_path, prog, args, env, input)
                                               .expect(format!("failed to exec `{}`", prog));
    dump_output(config, testfile, out, err);
    return ProcRes {
        status: status,
        stdout: out,
        stderr: err,
        cmdline: cmdline
    };
}
