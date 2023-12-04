{
  description = "Kadena's Pact smart contract language";

  inputs = {
    hs-nix-infra.url = "github:kadena-io/hs-nix-infra";
    flake-utils.url = "github:numtide/flake-utils";
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
      packages.pact-core-binary = flake.packages."pact-core:exe:repl";
      packages.pact-core-tests  = flake.packages."pact-core:test:core-tests";

      packages.default = packages.pact-core-binary;

      packages.recursive = with hs-nix-infra.lib.recursive system;
        wrapRecursiveWithMeta "pact-core" "${wrapFlake self}.default";

      inherit (flake) devShell;

      packages.check = pkgs.runCommand "check" {} ''
        export LANG=C.UTF-8

        echo ${mkCheck "pact-core" packages.default}

        echo ${packages.pact-core-tests}
        (cd ${self}; ${packages.pact-core-tests}/bin/core-tests)

        echo ${mkCheck "devShell" devShell}
        echo works > $out
      '';

      # Other flake outputs provided by haskellNix can be accessed through
      # this project output
      inherit project;
    });
}
