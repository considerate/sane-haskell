{
  inputs = {
    nixpkgs = {
      url = "github:NixOS/nixpkgs";
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
  };
  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      overlay = final: prev: {
        hsPkgs = final.haskell.packages.ghc8107.extend (hfinal: hprev: {
          sane-haskell = hfinal.callCabal2nix "sane-haskell" ./. { };
        });
      };
      pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [ overlay ];
        config = {
          allowUnfree = true;
          cudaSupport = true;
        };
      };
    in
    {
      devShells = {
        default = pkgs.hsPkgs.shellFor {
          name = "haskell-shell";
          packages = ps: [ ps.sane-haskell ];
          nativeBuildInputs = [
            pkgs.cabal-install
            pkgs.hsPkgs.haskell-language-server
          ];
        };
      };
    });
}
