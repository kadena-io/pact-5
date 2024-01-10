<p align="center">
<img src="https://i.imgur.com/bAZFAGF.png" width="450" height="243" alt="Kadena" title="Kadena">
</p>

<p>&nbsp;</p>

# The Pact Programming Language

[Pact](http://kadena.io/build) is an open-source, Turing-**in**complete smart contract language that has been purpose-built with blockchains first in mind. Pact focuses on facilitating transactional logic with the optimal mix of functionality in authorization, data management, and workflow.

Read the whitepaper:

- [The Pact Smart Contract Language](https://d31d887a-c1e0-47c2-aa51-c69f9f998b07.filesusr.com/ugd/86a16f_442a542b64554cb2a4c1ae7f528ce4c3.pdf)

For additional information, press, and development inquiries, please refer to the Kadena [website](https://kadena.io)

> [!IMPORTANT]
> This repository hosts a rewrite of the Pact language and is not utilized by [chainweb-node](https://github.com/kadena-io/chainweb-node), 
> serving primarily for local development and testing purposes. We detail the planned transition from Pact to Pact Core in [Section Roadmap](#roadmap).

If you are looking for legacy Pact, see [github.com/kadena-io/pact](https://github.com/kadena-io/pact).

## Table of Contents
  - [Quickstart](#quickstart)
  - [Pact Core vs Pact](#pact-core-vs-pact)
    - [Roadmap](#roadmap)
  - [Documentation](#documentation)
  - [Installing Pact Core](#installing-pact-core)
    - [Binary Downloads](#binary-downloads)
	- [Building from Source](#building-from-source)
	  - [Using the Nix Infrastructure (recommend)](#using-the-nix-infrastructure)
	  - [Using Cabal and GHC](#using-cabal-and-ghc)
  - [Editor Integration (Language Server)](#editor-integration)
  - [License](#license)

## Quickstart

1. Download the latest Pact binary from [Github Releases](https://github.com/kadena-io/pact-core/releases/latest).
2. Extract the `pact` binary, make sure that you have the required permissions to execute it.
   ```bash
   chmod +x /path/to/pact
   ```
3. (Optional) Add the path to your `$PATH` environment variable or adding the line to your shell profile.
   ```bash
   export PATH=$PATH:/path/to/
   ```
4. Execute `pact` and have fun :-)

## Pact Core vs Pact
Pact Core is a redevelopment of the Pact language, focusing on enhancing scalability, maintainability, and performance in response to increasingly complex demands from users and partners.

Pact Core enables sustainable growth of the Pact featureset within the Kadena ecosystem by offering a more modular and maintainable internals, enabling the community to further develop and propose enhancements to the language and components that rely on it.

Pact Core maintains semantic equivalence to Pact, aside from minor differences for security and performance. Existing code will behave the same as code written in (legacy) Pact. Breaking modifications will be communicated using the [Kadena Improvement Process](https://github.com/kadena-io/kips) (KIP) process.

### Roadmap
> [!NOTE]
> The current roadmap is being sketched by the Pact Team and will be updated appropriatly.

## Documentation
The [Kadena Docs](https://docs.kadena.io/pact) site serves as the primary source of information about Pact.
You can find information about how to get started with the Pact language, how to execute already deployed contracts, and follow
our step-by-step tutorials. 

We recommend that new users start with our [beginner`s guide](https://docs.kadena.io/pact/beginner), which provides an 
understanding of the fundamental concepts and terminology of the language.

## Installing Pact Core
To install Pact Core on your infrastructure, you have the option to download a pre-built binary or compile it from the source.

### Binary Downloads
You can obtain the latest released version of Pact from our GitHub releases page [here](https://github.com/kadena-io/pact-core/releases).
Ensure to download the binary that corresponds to your specific architecture.

### Building from Source
Two methods are supported for building Pact Core: using bare Cabal and GHC or employing the Nix package manager.

#### Using Cabal and GHC

Building is a process comprising four steps:
1. Install [GHCup](https://www.haskell.org/ghcup/)
2. Set and ensure the versions specified below are correctly set:
   ```bash
   ghcup install ghc 9.6.3 && ghcup install-cabal
   ```
3. Update Haskell packages:
   ```shell
   cabal update
   ```
4. Build the Pact binary:
   ```shell
   cabal build exe:pact
   ```

5. (Optional) You can either run `pact` directly:
   ```shell
   cabal run exe:pact
   ```
   or, if you prever to make the `pact` executable available in your `$PATH` environment, run:
   ```shell
   cabal install exe:pact
   ```

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


## Editor Integration

We offer built-in integration with Microsoft's [Language Server Protocol](https://microsoft.github.io/language-server-protocol/) (LSP).
Check your editor's support for the LSP protocol and the specific setup instructions. The server itself can be initiated as follows.

```shell
pact --lsp
```

> [!NOTE]
> We continue to add specifics on major editors such as Emacs, vim, and VSCode.

## License

This code is distributed under the terms of the BSD3 license. See [LICENSE](LICENSE) for details.
