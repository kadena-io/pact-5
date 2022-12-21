# Pact Core Checklist

## Todo: Update this to better reflect notion.so pact-core roadmap.

## Interpreter flow

- We receive source as strict `ByteString`, this is [lexed](./pact-core/Pact/Core/Syntax/Lisp/Lexer) and then [parsed](./pact-core/Pact/Core/Syntax/Lisp/Parser.y) into the data structures [here](./pact-core/Pact/Core/Syntax/Lisp/ParseTree.hs), in particular `ParseTree.Expr`. Line locations are provided by the `LineInfo` data type and are emitted by the lexer.
- From `ParseTree.Expr` which attempts to faithfully represent source syntax, we get rid of all syntactic sugar into an abstract syntax tree in the [Desugar](./pact-core/Pact/Core/IR/Desugar.hs) module.
- After desugaring into `IR.Term`, we perform name resolution also within `Desugar`, that is: we resolve all top-level scoped names into their respective fully qualified names (that is, a call `(foo 1)` referencing a module's function `m.foo` turns this call into `m.foo 1`, and a call `(foo 1)` referencing a let-bound variable `let foo = ... in (foo 1)` turns this into `(Bound(foo, 0) 1)` (with a debruijn index).
- After desugaring, we perform [type inference](./pact-core/Pact/Core/IR/Typecheck.hs) which returns a tree with all overloads (e.g `+`) resolved but not yet specialized (That is, we check whether an application of `+` references a valid instance overload of `+`, and this is propagated into the tree).
- After typechecking, we specialize all overloads aka `(+ 1 2)` becomes `(addInt 1 2)`.
- Here, we can optionally re-typecheck the tree (Todo: this needs to be implemented/checked for all overloads) via the system f typechecker.
- After the above step ^, we strip the tree of types into [untyped term](./pact-core/Pact/Core/Untyped/Term.hs), and can then pick an evaluator to compute the value of the program.

### Special notes

- The loaded builtins for the term are abstracted out into a type vairbale

### Source -> Typed
- [x] Parsing new syntax (Term)
- [x] Renamer to locally nameless for terms
- [x] Type inference for Term
- [ ] Core IR Modules (Parsing, Tc)
- [ ] Core IR Type inference support
- [x] IR to Typed Core in typechecker
- [ ] Typeclass overload resolution

#### Optional
- [ ] Interpreter for IR


## Typed
- [x] Core Typed IR
- [x] Core Type language (Note: potentially `Type` should be 2 different types, for IR and Core)
- [x] Renaming to locally nameless
- [x] Type checking for typed Core
- [x] CEK Interpreter for Typed corew

## Untyped

- [ ] CEK for untyped core
- [ ] Optimizations/Jit

# Features
- [ ] Capabilities Support
- [ ] Defpact Support

## Low Prio

- [ ] Constant folding / Propagation
- [ ] JIT

# General Compiler flow

## Source on chain version:

Source --> Name resolution + renaming -> Typecheck (Gas!)
\+ Overload resolution -> Typed core -> Sanity typecheck -> Untyped + onchain persist + execute.

## Typed Core on chain version:

### Off-chain
Source (Frontend of choice) --> Name reso + renaming -> Typecheck (No gas :)
\-> Overload resolution -> Typed Core -> Optional (Optimization)

### On-chain
Typed Core Source --> Name resolution + renaming -> Typecheck (Gas) -> Untyped + onchain persist + execute.
