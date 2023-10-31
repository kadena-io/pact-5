{
  description = "Kadena's Pact smart contract language";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?rev=4d2b37a84fad1091b9de401eb450aae66f1a741e";
    # nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem
      [ "x86_64-linux" "x86_64-darwin"
        "aarch64-linux" "aarch64-darwin"
      ] (system:
    let
      pkgs = import nixpkgs {
        inherit system overlays;
        inherit (haskellNix) config;
      };
      flake = pkgs.pact-core.flake {
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
    in pkgs.lib.recursiveUpdate flake rec {
      packages.pact-core-binary = flake.packages."pact-core:exe:repl";
      packages.pact-core-tests  = flake.packages."pact-core:test:core-tests";

      packages.default = packages.pact-core-binary;

      devShell = pkgs.haskellPackages.shellFor {
        packages = p: [
        ];

        buildInputs = with pkgs.haskellPackages; [
          cabal-install
          haskell-language-server
        ];

        withHoogle = true;
      };
      packages.check = pkgs.runCommand "check" {} ''
        export LANG=C.UTF-8

        echo ${mkCheck "pact-core" packages.default}

        echo ${packages.pact-core-tests}
        (cd ${self}; ${packages.pact-core-tests}/bin/core-tests)

        echo ${mkCheck "devShell" flake.devShell}
        echo works > $out
      '';
    });
}
