# Builds the ltext executable. Without flakes: `nix-build`.
{ pkgs ? import <nixpkgs> { }
, lib ? pkgs.lib
, rustPlatform ? pkgs.rustPlatform
  # Git commit being built, if known. Without it, `ltext --version` reports the
  # Cargo.toml version (right for builds of a release tag).
, rev ? null
}:

let
  cargoToml = lib.importTOML ./Cargo.toml;
in
rustPlatform.buildRustPackage {
  pname = cargoToml.package.name;
  version = cargoToml.package.version;

  src = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [
      ./Cargo.toml
      ./Cargo.lock
      ./build.rs
      ./src
      ./tests
      ./benches
      ./golden
    ];
  };

  cargoLock.lockFile = ./Cargo.lock;

  env = lib.optionalAttrs (rev != null) { LTEXT_GIT_REV = rev; };

  # Unit and property tests run by default; also check the golden corpus.
  postCheck = ''
    patchShebangs golden/run.sh
    golden/run.sh target/*/release/ltext
  '';

  meta = {
    description = cargoToml.package.description;
    homepage = "https://github.com/ltext/ltext";
    license = lib.licenses.bsd3;
    mainProgram = "ltext";
  };
}
