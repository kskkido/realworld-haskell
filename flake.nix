{
  description = "Fullstack conduit app";
  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/nixos-unstable";
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
    easy-ps = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
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
  outputs = { self, nixpkgs, flake-utils, easy-ps, ... }:
    flake-utils.lib.eachSystem [ "x86_64-darwin" ] (system:
      let
        ghc = "ghc925";
        overlay = self: super: {
          haskell = super.haskell // {
            packages = super.haskell.packages // {
              ${ghc} = super.haskell.packages.${ghc}.extend (self: super: {
                honduit-api = self.callCabal2nix "honduit-api" ./packages/honduit-api {};
                honduit-server = self.callCabal2nix "honduit-server" ./packages/honduit-server {};
              });
            };
          };
          purescriptPackages = import easy-ps {
            pkgs = self;
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
            honduit-api = haskell.packages.${ghc}.honduit-api;
            honduit-server = haskell.packages.${ghc}.honduit-honduit-server;
            default = packages.honduit-server;
          };
          devShell = mkShell {
            buildInputs = [
              docker
              nodejs-18_x
              postgresql_14
              purescriptPackages.purs-0_14_3
              purescriptPackages.spago
              ( haskell.packages.${ghc}.ghcWithHoogle (hpkgs: [
                  hpkgs.honduit-api
                  hpkgs.honduit-server
                  hpkgs.cabal-install
                  haskell-language-server
                ])
              )
            ];
          shellHook =
            ''
              echo "Hello shell"
              BUILD_ENV=$(cat configs/local/.env)
              export $(echo $BUILD_ENV | xargs)
            '';
          };
        }
    );
}

