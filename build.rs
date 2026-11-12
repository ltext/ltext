//! Works out the version string that `ltext --version` prints. In order:
//!
//! 1. `LTEXT_VERSION`, if set. Packagers can use this to override the version.
//! 2. `git describe`, when building from ltext's own git checkout. A build of
//!    tag `v0.2.0` prints `0.2.0`. A build of any other commit prints something
//!    like `0.2.0-3-g1a2b3c4`, meaning 3 commits after v0.2.0 at commit
//!    1a2b3c4, with `-dirty` added if there are uncommitted changes. With no
//!    tags fetched, it prints just the commit hash.
//! 3. `<Cargo.toml version>+g<rev>`, when `LTEXT_GIT_REV` gives the commit but
//!    there's no git checkout. The Nix flake does this.
//! 4. The version in Cargo.toml, e.g. for a crates.io or release-tarball build.

use std::path::{Path, PathBuf};
use std::process::Command;

fn main() {
    println!("cargo:rerun-if-env-changed=LTEXT_VERSION");
    println!("cargo:rerun-if-env-changed=LTEXT_GIT_REV");

    let manifest_dir = PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap());
    let cargo_version = std::env::var("CARGO_PKG_VERSION").unwrap();

    let version = env_nonempty("LTEXT_VERSION")
        .or_else(|| git_describe(&manifest_dir))
        .or_else(|| env_nonempty("LTEXT_GIT_REV").map(|rev| format!("{cargo_version}+g{rev}")))
        .unwrap_or(cargo_version);

    println!("cargo:rustc-env=LTEXT_VERSION={version}");
}

fn env_nonempty(name: &str) -> Option<String> {
    std::env::var(name).ok().filter(|v| !v.trim().is_empty())
}

fn git(dir: &Path, args: &[&str]) -> Option<String> {
    let out = Command::new("git")
        .arg("-C")
        .arg(dir)
        .args(args)
        .output()
        .ok()?;
    if !out.status.success() {
        return None;
    }
    let s = String::from_utf8(out.stdout).ok()?.trim().to_owned();
    (!s.is_empty()).then_some(s)
}

fn git_describe(dir: &Path) -> Option<String> {
    // Only trust git when the checkout is ltext itself, not some other
    // repository that happens to contain this crate (e.g. a vendor directory).
    let top = PathBuf::from(git(dir, &["rev-parse", "--show-toplevel"])?);
    if top.canonicalize().ok()? != dir.canonicalize().ok()? {
        return None;
    }

    // Re-run when HEAD moves, tags change, or sources are edited (for -dirty).
    if let Some(git_dir) = git(dir, &["rev-parse", "--absolute-git-dir"]) {
        println!("cargo:rerun-if-changed={git_dir}/HEAD");
        println!("cargo:rerun-if-changed={git_dir}/index");
    }
    if let Some(common) = git(dir, &["rev-parse", "--git-common-dir"]) {
        let common = dir.join(common);
        for p in ["refs", "packed-refs"] {
            println!("cargo:rerun-if-changed={}", common.join(p).display());
        }
    }
    for p in ["src", "Cargo.toml", "Cargo.lock", "build.rs"] {
        println!("cargo:rerun-if-changed={p}");
    }

    let d = git(
        dir,
        &[
            "describe", "--tags", "--match", "v[0-9]*", "--always", "--dirty",
        ],
    )?;
    Some(d.strip_prefix('v').unwrap_or(&d).to_owned())
}
