<p align="center">
<img src="https://i.imgur.com/bAZFAGF.png" width="450" height="243" alt="Kadena" title="Kadena">
</p>

<p>&nbsp;</p>

# The Pact Programming Language

[Pact](http://kadena.io/build) is an open-source, Turing-**in**complete smart contract language that has been purpose-built with blockchains first in mind. Pact focuses on facilitating transactional logic with the optimal mix of functionality in authorization, data management, and workflow.

Read the whitepaper:

- [The Pact Smart Contract Language](https://d31d887a-c1e0-47c2-aa51-c69f9f998b07.filesusr.com/ugd/86a16f_442a542b64554cb2a4c1ae7f528ce4c3.pdf)

For additional information, press, and development inquiries, please refer to the Kadena [website](https://kadena.io)

[!IMPORTANT]
This repository hosts a rewrite of the Pact language and is not utilized by [chainweb-node](https://github.com/kadena-io/chainweb-node), 
serving primarily for local development and testing purposes. We detail the planned transition from Pact to Pact Core in [Section Roadmap](#roadmap).

￼
￼
￼
￼


## Table of Contents

  - [Pact Core and Pact](#pact-core-and-pact)
    - [Roadmap](#roadmap)
  - [Documentation](#documentation)
  - [Quickstart](#quickstart)
  - [Installing Pact](#installing-pact-core)
    - [Binary Downloads](#binary-downloads)
	- [Building from Source](#building-from-source)
	  - [Using the Nix Infrastructure (recommend)](#using-the-nix-infrastructure)
	  - [Using Cabal and GHC](#using-cabal-and-ghc)
  - [Editor Integration (Language Server)](#editor-integration)
  - [License](#license)


## Pact Core and Pact

### Roadmap


## Documentation

The [Kadena Docs](https://docs.kadena.io) site serves as the primary source of information about Kadena products.
You can find information about how to get started with the Pact language, how to execute already deployed contracts, and follow
our step-by-step tutorials. 

## Quickstart

To quickly begin exploring the Pact language, download the corresponding binary from the
latest release, see [Binary Downloads](#binary-downloads).

We recommend that new users start with our [beginner`s guide](https://docs.kadena.io/pact/beginner), which provides an 
understanding of the fundamental concepts and terminology of the language.

If you prefer a more hands-on approach, download the Pact binaries and execute the REPL.
This will allow you to begin evaluating expressions, such as `(+ 1 0)`, immediately.

## Installing Pact Core

To install Pact on your infrastructure, you have the option to download a pre-built binary or compile it from the source.

### Binary Downloads

You can obtain the latest released version of Pact from our GitHub releases page [here](https://github.com/kadena-io/pact-core/releases).
Ensure to download the binary that corresponds to your specific architecture.

### Building from Source
We recommend using [Nix]() to build pact-core from sources.
Alternatively, you can use [Cabal](https://www.haskell.org/cabal/) along with a properly set up Haskell compiler ([GHC](https://www.haskell.org/ghc/)) as the default approach.

#### Using the Nix Infrastructure
Kadena offers a binary cache for all Nix builds, allowing users to accelerate their build times by utilizing our cache infrastructure.
The specifics of setting up the cache depend on various factors and are beyond the scope of this instruction.
A good starting point for configuring our cache is the Nix documentation on binary caches, available at: [NixOS Wiki on Binary Cache](https://nixos.wiki/wiki/Binary_Cache).

The binary cache is typically configured using the `nixConfig` attribute in our flake definition as follows:

```
nixConfig = {
    extra-substituters = "https://nixcache.chainweb.com https://cache.iog.io";
    trusted-public-keys = "nixcache.chainweb.com:FVN503ABX9F8x8K0ptnc99XEz5SaA4Sks6kNcZn2pBY= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=";
  };
```

Executing `nix build` within the root directory of this project will create a build under the `./result` symbolic link.
This will contain the artifact within its `bin` directory.

Entering the developer shell using `nix develop` will bring all required dependencies into scope, enabling the use of
`cabal build` to compile the final project.

#### Using Cabal and GHC

To build Pact core using Cabal and GHC directly, we recommend using [GHCup](https://www.haskell.org/ghcup/) to set up the corresponding versions:
- Cabal version 3.0 or higher
- GHC version 9.6 or higher

After updating local packages with `cabal update`, the project can be built using `cabal build`.
￼

## Editor Integration

## License

This code is distributed under the terms of the BSD3 license. See [LICENSE](LICENSE) for details.
