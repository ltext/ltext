{
  description = "λtext - general-purpose text-file templating utility";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        ltext = pkgs.callPackage ./default.nix { };
      in {
        packages = {
          inherit ltext;
          default = ltext;
        };

        apps.default = flake-utils.lib.mkApp { drv = ltext; };

        devShells.default = pkgs.mkShell {
          inputsFrom = [ ltext ];
          packages = with pkgs; [ cargo rustc clippy rustfmt rust-analyzer ];
        };
      });
}
