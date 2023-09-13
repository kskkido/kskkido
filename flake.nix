{
  description = "kskkido";
  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/nixos-unstable";
    };
    npmlock2nix = {
      url = "github:winston0410/npmlock2nix/issue113";
      flake = false;
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
    gitignore = {
      url = "github:hercules-ci/gitignore.nix";
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
    };
  };
  outputs = { self, nixpkgs, npmlock2nix, flake-utils, ... }:
    flake-utils.lib.eachSystem [ "x86_64-darwin" ] (system:
      let
        ghc = "ghc925";
        overlay = self: super: {
          haskell = super.haskell // {
            packages = super.haskell.packages // {
              ${ghc} = super.haskell.packages.${ghc}.extend (self: super: {
              });
            };
          };
        };
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            overlay
          ];
        };
      in
        with pkgs; rec {
          packages = rec {
            kskkido-blog = haskell.packages.${ghc}.callCabal2nix "kskkido-blog" ./packages/kskkido-blog {};
            kskkido-blog-server = haskell.packages.${ghc}.callCabal2nix "kskkido-blog-server" ./packages/kskkido-blog-server {};
            default = kskkido-blog;
          };
          devShell = mkShell {
            buildInputs = [
              cabal-install
              gmp
              zlib
              docker
              nodejs-18_x
              ( haskell.packages.${ghc}.ghcWithHoogle (hpkgs: [
                  haskell-language-server
                ])
              )
            ];
          shellHook =
            ''
              echo "Hello shell"
              BUILD_ENV=$(cat ./configs/local/.env)
              export $(echo $BUILD_ENV | xargs)
            '';
          };
        }
    );
}

