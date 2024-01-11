{
  description = "Kadena's Pact smart contract language";

  inputs = {
    hs-nix-infra.url = "github:kadena-io/hs-nix-infra";
    flake-utils.url = "github:numtide/flake-utils";
  };
  
  nixConfig = {
    extra-substituters = "https://nixcache.chainweb.com https://cache.iog.io";
    trusted-public-keys = "nixcache.chainweb.com:FVN503ABX9F8x8K0ptnc99XEz5SaA4Sks6kNcZn2pBY= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=";
  };
  
  outputs = { self, flake-utils, hs-nix-infra }:
    flake-utils.lib.eachSystem
      [ "x86_64-linux" "x86_64-darwin"
        "aarch64-linux" "aarch64-darwin"
      ] (system:
    let
      inherit (hs-nix-infra) haskellNix nixpkgs;
      pkgs = import nixpkgs {
        inherit system overlays;
        inherit (haskellNix) config;
      };
      project = pkgs.pact-core;
      flake = project.flake {
      };
      overlays = [ haskellNix.overlay
        (final: prev: {
          pact-core =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc962";
              shell.tools = {
                cabal = {};
                haskell-language-server = {};
              };
              shell.buildInputs = with pkgs; [
                zlib
                z3
                pkgconfig
              ];
            };
        })
      ];
      # This package depends on other packages at buildtime, but its output does not
      # depend on them. This way, we don't have to download the entire closure to verify
      # that those packages build.
      mkCheck = name: package: pkgs.runCommand ("check-"+name) {} ''
        echo ${name}: ${package}
        echo works > $out
      '';
    in rec {
      packages.pact-core-binary = flake.packages."pact-core:exe:pact";
      packages.pact-core-gasmodel = flake.packages."pact-core:exe:gasmodel";
      packages.pact-core-tests = flake.checks."pact-core:test:core-tests".overrideAttrs (old: {
        PACT_CORE_NIXBUILD=packages.pact-core-binary.exePath;
      });
      
      packages.default = packages.pact-core-binary;

      packages.recursive = with hs-nix-infra.lib.recursive system;
        wrapRecursiveWithMeta "pact-core" "${wrapFlake self}.default";

      inherit (flake) devShell;

      packages.check = pkgs.runCommand "check" {} ''
        echo ${mkCheck "pact-core" packages.default}

        echo ${mkCheck "pact-core" packages.pact-core-gasmodel}

        echo ${mkCheck "pact-core-test" packages.pact-core-tests} 

        echo ${mkCheck "devShell" devShell}
        echo works > $out
      '';

      # Other flake outputs provided by haskellNix can be accessed through
      # this project output
      inherit project;
    });
}
