{
  description = "Kadena's Pact smart contract language";

  inputs = {
    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };

    hs-nix-infra = {
      url = "github:kadena-io/hs-nix-infra";
      inputs.hackage.follows = "hackage";
    };

    flake-utils.url = "github:numtide/flake-utils";
    nix-bundle-exe = { url = "github:3noch/nix-bundle-exe"; flake = false; };
  };

  nixConfig = {
    extra-substituters = "https://nixcache.chainweb.com https://cache.iog.io";
    trusted-public-keys = "nixcache.chainweb.com:FVN503ABX9F8x8K0ptnc99XEz5SaA4Sks6kNcZn2pBY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=";
  };

  outputs = inputs@{ self, flake-utils, hs-nix-infra, ... }:
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
      project = pkgs.pact-tng;
      flake = project.flake {
      };
      overlays = [ haskellNix.overlay
        (final: prev: {
          pact-tng =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc964";
              shell.tools = {
                cabal = {};
                haskell-language-server = {};
              };
              shell.buildInputs = with pkgs; [
                zlib
                z3
                pkg-config
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
      packages.pact-binary = flake.packages."pact-tng:exe:pact";
      packages.pact-binary-bundle = pkgs.callPackage inputs.nix-bundle-exe {}
        packages.pact-binary;
      packages.pact-gasmodel = flake.packages."pact-tng:exe:gasmodel";
      packages.pact-tests = flake.checks."pact-tng:test:core-tests".overrideAttrs (old: {
        PACT_CORE_NIXBUILD=packages.pact-binary.exePath;
      });

      packages.default = packages.pact-binary;

      packages.recursive = with hs-nix-infra.lib.recursive system;
        wrapRecursiveWithMeta "pact-tng" "${wrapFlake self}.default";

      inherit (flake) devShell;

      packages.check = pkgs.runCommand "check" {} ''
        echo ${mkCheck "pact" packages.default}

        echo ${mkCheck "pact-gasmodel" packages.pact-gasmodel}

        echo ${mkCheck "pact-tests" packages.pact-tests}

        echo ${mkCheck "devShell" devShell}
        echo works > $out
      '';

      # Other flake outputs provided by haskellNix can be accessed through
      # this project output
      inherit project;
    });
}
