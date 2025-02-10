<p align="center">
<img src="https://cdn.sanity.io/images/agrhq0bu/production/3914c91a76778ba6b2c774a8fb0c751272377cbb-2918x672.png" alt="Kadena" title="Kadena">
</p>

<p>&nbsp;</p>

# Pact: Smart Contract Programming Language

[Pact](http://kadena.io/build) is an open-source, smart contract language designed to execute transactional logic efficiently in a blockchain environment. 
The Pact smart contract programming language is intentionally **Turing-incomplete** to prevent recursion and unbounded looping that could be exploited or disrupt the blockchain network. 
The Pact language focuses on providing smart contract authors with the right set of features to optimize security, performance, and transparency while providing flexible methods for authorizing access to contract data and function, managing data storage, and designing application workflows.

For a historical perspective on the design of Pact, see [The Pact Smart Contract Language](https://www.kadena.io/whitepapers) whitepaper.

For additional information about Pact and development resources, visit the [Kadena website](https://kadena.io).

## Table of contents
- [Pact: Smart Contract Programming Language](#pact-smart-contract-programming-language)
  - [Table of contents](#table-of-contents)
  - [Quick start](#quick-start)
  - [Pact 5 and previous releases of Pact](#pact-5-and-previous-releases-of-pact)
  - [Documentation](#documentation)
  - [Installation options](#installation-options)
    - [Build from source using Cabal and GHC](#build-from-source-using-cabal-and-ghc)
    - [Build from source using Nix](#build-from-source-using-nix)
  - [Editor integration](#editor-integration)
  - [License](#license)

## Quick start

To get started with Pact:

1. Download the latest Pact binary for your operating system using Homebrew or from [Github Releases](https://github.com/kadena-io/pact-5/releases/tag/5.0).

2. Extract the `pact` binary, if necessary, and make sure that you have permissions to execute it:
   
   ```bash
   chmod +x /path/to/pact
   ```

3. (Optional) Add the path to your `$PATH` environment variable or add the following line to your shell profile:
   
   ```bash
   export PATH=$PATH:/path/to/
   ```

4. Verify the installation by checking the Pact version:

   ```bash
   pact --version
   ```

5. View usage information by running the following command:
   
   ```bash
   pact --help
   ```

For additional information about installing Pact on a specific operating system, see [Installation and setup]((https://docs.kadena.io/smart-contracts/install)).

## Pact 5 and previous releases of Pact

The Pact 5 release is a comprehensive rewrite of the core elements of the Pact language.
This release of the Pact language retains functional parity with previous Pact releases, but with significant changes that enhance the scalability, maintainability, and performance of the language.
With this update, Pact 5 is better positioned to handle increasingly complex demands from community builders and development partners.
This new implementation also enables sustainable growth of the Pact feature set within the Kadena ecosystem.
By offering more modular and maintainable internal structures, Pact 5 can now better support community participation and enhancements to the language and components that rely on it. 

Although Pact 5 includes significant modifications and enhancements, it maintains semantic equivalence to previous Pact implementations.
If you're upgrading from a previous release of Pact, your existing code will continue to function as expected. 
If there are breaking changes in future releases, they will be communicated using the [Kadena Improvement Process](https://github.com/kadena-io/kips) (KIP) process.

## Documentation

The [Kadena Docs](https://docs.kadena.io/) site serves as the primary source of information for learning about and developing with Pact.
You can find information about how to get started with the Pact language, how to execute already deployed contracts, and how to perform common tasks
in step-by-step tutorials.
For a more guided approach to learning Pact, explore the [Zero to Pact](https://academy.kadena.io/kadena_course/zero-to-pact/) course in the [Kadena Academy](https://academy.kadena.io/).

For an introduction to fundamental concepts and terminology of the Pact language, see [Get started: An Introduction to Pact](https://docs.kadena.io/smart-contracts/get-started-intro).

## Installation options

You can download and install the Pact programming language and command-line interpreter locally on your local computer from prebuilt platform-specific binaries or build Pact directly from its source code. 
You can also run Pact in a browser in the development network if you set up a local node using the `kadena/devnet` Docker image.

To download a pre-built binary get the latest released version of Pact from [GitHub releases](https://github.com/kadena-io/pact-5/releases).
To build from source code, you can compile the code using Cabal and GHC or by using the Nix package manager.

### Build from source using Cabal and GHC

To build from the source code using Cabal and GHC:

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

5. Run `pact` directly or add it to your $PATH environment variable.

   To run `pact` directly:
   
   ```shell
   cabal run exe:pact
   ```

   To make the `pact` executable available in your `$PATH` environment:
   ```shell
   cabal install exe:pact
   ```

### Build from source using Nix

Kadena offers a binary cache for all Nix builds, allowing users to accelerate their build times by using the cache infrastructure.
The specifics of setting up the cache depend on various factors and are beyond the scope of this instruction.
A good starting point for configuring the cache is the Nix documentation on binary caches, available at: [NixOS Wiki on Binary Cache](https://nixos.wiki/wiki/Binary_Cache).

The binary cache is typically configured using the `nixConfig` attribute in the flake definition as follows:

```
nixConfig = {
    extra-substituters = "https://nixcache.chainweb.com https://cache.iog.io";
    trusted-public-keys = "nixcache.chainweb.com:FVN503ABX9F8x8K0ptnc99XEz5SaA4Sks6kNcZn2pBY= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=";
  };
```

Executing `nix build` within the root directory of this project creates a build under the `./result` symbolic link.
After building, the binary is located in `./result/bin` directory.

Start a developer shell by running `nix develop` to bring all of the required dependencies into scope, enabling the use of
`cabal build` to compile the final project.

## Editor integration

Pact includes built-in support for the Microsoft [Language Server Protocol](https://microsoft.github.io/language-server-protocol/) (LSP).
You should check your code editor for LSP support and editor-specific setup instructions. 
The server itself can be initiated as follows:

```shell
pact --lsp
```

Pact currently supports the following LSP features:

- Document diagnostics
- Hover information (Built-ins and user specified docs)
- Jump to definition (Top-level)

> [!NOTE]
> We continue to add specifics on major editors such as Emacs, vim, and VSCode.

## License

This code is distributed under the terms of the BSD3 license. See [LICENSE](LICENSE) for details.
