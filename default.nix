# Non-flake entry point: `nix-build` builds the ltext executable.
# Prefer `nix build` with flakes enabled.
{ pkgs ? import <nixpkgs> { } }:
pkgs.haskell.lib.justStaticExecutables
  (pkgs.haskellPackages.callCabal2nix "ltext" ./. { })
