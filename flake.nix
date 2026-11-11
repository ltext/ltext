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
        hsPkgs = pkgs.haskellPackages;
        ltext = hsPkgs.callCabal2nix "ltext" self { };
      in {
        packages = {
          inherit ltext;
          default = pkgs.haskell.lib.justStaticExecutables ltext;
        };

        apps.default = flake-utils.lib.mkApp { drv = self.packages.${system}.default; };

        devShells.default = hsPkgs.shellFor {
          packages = _: [ ltext ];
          withHoogle = false;
          nativeBuildInputs = [
            hsPkgs.cabal-install
            hsPkgs.haskell-language-server
            pkgs.hpack
          ];
        };
      });
}
