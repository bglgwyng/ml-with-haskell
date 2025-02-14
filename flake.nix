{
  nixConfig = {
    extra-substituters = [
      "https://hasktorch.cachix.org"
    ];
    extra-trusted-public-keys = [
      "hasktorch.cachix.org-1:wLjNS6HuFVpmzbmv01lxwjdCOtWRD8pQVR3Zr/wVoQc="
    ];
  };

  inputs = {
    hasktorch.url = "github:hasktorch/hasktorch";
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.follows = "hasktorch/nixpkgs";
  };

  outputs =
    inputs @ { self
    , hasktorch
    , nixpkgs
    , flake-parts
    , ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      perSystem =
        { config
        , system
        , pkgs
        , ...
        }:
        let
          ghc = "ghc965";
          haskellPackages = pkgs.haskell.packages.${ghc};
        in
        rec {
          _module.args = {
            pkgs = import nixpkgs {
              inherit system;
              # Set to false to disable CUDA support
              config.cudaSupport = true;
              overlays = [
                hasktorch.overlays.default
              ];
            };
          };
          packages.default =
            haskellPackages.callCabal2nix "ml-with-haskell" ./ml-with-haskell { };
          devShells.default = haskellPackages.shellFor {
            packages = ps: [ packages.default ];
            nativeBuildInputs = with pkgs; [
              cabal-install
              haskell-language-server
            ];
            withHoogle = true;
          };
        };
    };
}
