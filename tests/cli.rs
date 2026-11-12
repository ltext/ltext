//! `--version` depends on how the binary was built (see build.rs), so it's
//! checked here instead of in the golden corpus.

use std::process::Command;

#[test]
fn version_flag_reports_build_version() {
    for flag in ["--version", "-V"] {
        let out = Command::new(env!("CARGO_BIN_EXE_ltext"))
            .arg(flag)
            .output()
            .unwrap();
        assert!(out.status.success());
        assert_eq!(
            String::from_utf8(out.stdout).unwrap(),
            format!("ltext {}\n", env!("LTEXT_VERSION"))
        );
    }
}

#[test]
fn version_is_cargo_version_or_describes_a_commit() {
    let v = env!("LTEXT_VERSION");
    let cargo = env!("CARGO_PKG_VERSION");
    // A release, `git describe` output (… -g<hash>, maybe -dirty), a bare
    // commit hash, or <version>+g<hash>.
    let is_hash = |s: &str| s.len() >= 7 && s.chars().all(|c| c.is_ascii_hexdigit());
    let base = v.strip_suffix("-dirty").unwrap_or(v);
    let ok = v == cargo
        || is_hash(base)
        || base
            .rsplit_once("-g")
            .or_else(|| base.rsplit_once("+g"))
            .is_some_and(|(_, h)| is_hash(h));
    assert!(ok, "unexpected version string {v:?}");
}
