# Pact Semantics document

This document should serve as a record of known pact semantics, covering as much of the language as possible, including corner cases, and semantic divergences between core and prod pact.

Bullet points that are not filled are potential tasks

## Modules

### Module Lexing and Parsing
- [x] `module` keyword
- [x] Governance forms: keyed governance (via Keyset name string) or cap governance bare name resolution
- [x] use, bless and implements forms
- [x] defuns, defconsts, defcaps
- [x] defschema, deftable
- [x] defpacts

### Module name resolution
- [x] Defuns
- [x] DefConsts
- [x] DefCaps
- [x] DefSchemas
- [x] DefTables
- [ ] Defpacts
- [ ] Internal defs produce the right dependency tree (Needs test)


### Module evaluation semantics
- [x] Module governance eval (Note: the way we do this via interpretation is still not concrete)
  - [x] Cap governance
  - [x] Keyset Governance

## Top level forms

### Defuns
- [x] Defun name resolution
- [x] Defconst name resolution
- [x] Defcap name resolution
- [x] DefSchema name resolution

## Environment

### EvalEnv
- [x] Message signatures
- [x] Pactdb in env
- [x] Tx hash
- [x] Execution Mode
- [ ] Pact Step
- [x] Chain data
- [ ] Gas ref (In eval env? Unsure whether this will be abstracted out via `MonadGas`)
- [ ] Gas Env? Also not sure, see above ^
- [ ] Runtime Flags
- [ ] Warnings
- [ ] SPV Support
- [x] Purity (now in pactdb)

### EvalState
- [x] Loaded names (called `Loaded` currently instead of `RefStore` and `RefState`)
- [x] Call stack
- [x] Capabilities environment
- [ ] Gas log
- [x] Pact Events
- [ ] Defpact environment  `PactExec`

### Environment semantic changes
- RefStore no longer in `EvalEnv`. We do not resolve natives that way, but we potentially could for repl natives in modules. It is TBD
- `InRepl` flag is not necessary currently. This may change, but it serves as a way to fork on-chain errors in prod-pact.

## Intepreter
- [ ] Stable interpreter API
- [ ] EvalResult and EvalInput-type API


## Base Builtins

### at
- [x] Object case implemented
- [x] List case implemented
- [ ] Gas

### base64-decode
- [x] implemented
- [ ] Gas

### base64-encode
- [x] implemented
- [ ] Gas

### bind
- [x] implemented
- [ ] Gas

### chain-data
- [x] implemented
- [ ] Gas

### compose
- [x] implemented
- [ ] Gas

### concat
- [x] implemented
- [ ] Gas

### constantly
- [x] implemented
- [ ] Gas

### contains
- [x] String case implemented
- [x] List case implemented
- [x] Object case implemented
- [ ] Gas

### define-namespace
- [x] implemented
- [x] repl test
- [ ] Gas

### distinct
- [x] implemented
- [ ] Gas

### enforce
- [x] implemented
- [ ] Gas


### enforce-one
- [x] implemented
- [ ] Gas


### enforce-pact-version
TODO: Do we want this still???

### enumerate
- [x] implemented 2-arg overload
- [x] implemented 3-arg overload
- [ ] Gas

### filter
- [x] implemented
- [ ] Gas


### fold
- [x] implemented
- [ ] Gas

### format
- [x] implemented
- [ ] Gas

### hash
- [x] implemented
- [ ] Gas

### identity
- [x] implemented
- [ ] Gas

### if
- [x] implemented
- [ ] Gas

### int-to-str
- [x] implemented
- [ ] Gas


### is-charset
- [x] implemented
- [ ] Gas

### length
- [x] object case implemented
- [x] string case implemented
- [x] list case implemented
- [ ] Gas

### list
DELETED FROM EXISTENCE

### list-modules
(potentially) DELETED FROM EXISTENCE

### make-list
- [x] implemented
- [ ] Gas

### map
- [x] implemented
- [ ] Gas

### pact-id
- [x] implemented
- [ ] Gas

### pact-version
(potentially) DELETED FROM EXISTENCE

### public-chain-data
- Todo: provide in base loaded

### read-decimal
- [x] implemented
- [ ] Gas

### read-integer
- [x] implemented
- [ ] Gas

### read-msg
- [x] implemented both overloads
- [ ] Gas

### read-string
- [x] implemented
- [ ] Gas

### remove
- [x] implemented
- [ ] Gas

### resume
- [x] implemented
- [ ] Gas

### reverse
- [x] implemented
- [ ] Gas

### reverse
- [x] list case implemented
- [x] object case implemented
- [ ] Gas

### str-to-int
- [x] list case implemented
- [x] object case implemented
- [ ] Gas

### str-to-list
- [x] implemented
- [ ] Gas

### take
- [x] string case implemented
- [x] list case implemented
- [ ] Gas

### try
- [x] implemented
- [ ] Gas

### tx-hash
- [x] implemented
- [ ] Gas

### typeof
- [x] implemented
- [ ] Gas

### where
- [x] implemented
- [ ] Gas

### yield
- [x] implemented both overloads (yield and yield to chain)
- [ ] Gas

### zip
- [x] implemented both overloads (yield and yield to chain)
- [ ] Gas

### create-table
- [x] Local Bypass Check (NOTBYPASSED)
- [x] Enforcing blessed hashes
- [x] Guarded db access (Module admin or calledByModule)
- [x] Mock PactDb implementation
- [ ] ChainwebPactDb implementation
- [x] implemented
- [ ] Gas

### describe-keyset
- [x] Enforced top level only
- [x] Mock PactDb implementation
- [ ] ChainwebPactDb implementation
- [x] implemented
- [ ] Gas

### describe-module
- [x] Local only
- [x] Enforced top level only
- [x] Mock PactDb implementation
- [ ] ChainwebPactDb implementation
- [x] implemented
- [ ] Gas

### fold-db
- [x] Local Bypass Check  (BYPASS)
- [x] Enforcing blessed hashes
- [x] Guarded db access (Module admin or calledByModule)
- [x] Mock PactDb implementation
- [ ] ChainwebPactDb implementation
- [x] implemented
- [ ] Gas

### insert
- [x] Local Bypass Check (NOTBYPASSED)
- [x] Enforcing blessed hashes
- [x] Guarded db access (Module admin or calledByModule)
- [x] Mock PactDb implementation
- [ ] ChainwebPactDb implementation
- [x] implemented
- [ ] Gas

### keylog
- [x] Local only
- [x] Local Bypass Check (BYPASS)
- [x] Enforcing blessed hashes
- [x] Guarded db access (Module admin or calledByModule)
- [x] Mock PactDb implementation
- [ ] ChainwebPactDb implementation
- [x] implemented
- [ ] Gas

### keys
- [x] Local Bypass Check (BYPASS)
- [x] Enforcing blessed hashes
- [x] Guarded db access (Module admin or calledByModule)
- [x] Mock PactDb implementation
- [ ] ChainwebPactDb implementation
- [x] implemented
- [ ] Gas

### read
- [x] Local Bypass Check (BYPASS)
- [x] Enforcing blessed hashes
- [x] Guarded db access (Module admin or calledByModule)
- [x] Mock PactDb implementation
- [ ] ChainwebPactDb implementation
- [x] implemented
- [ ] Gas

### select
- [x] Local Bypass Check (BYPASS)
- [x] Enforcing blessed hashes
- [x] Guarded db access (Module admin or calledByModule)
- [x] Mock PactDb implementation
- [ ] ChainwebPactDb implementation
- [x] implemented
- [ ] Gas

### txids
- [x] Local only
- [x] Local Bypass Check (BYPASS)
- [x] Enforcing blessed hashes
- [x] Guarded db access (Module admin or calledByModule)
- [x] Mock PactDb implementation
- [ ] ChainwebPactDb implementation
- [x] implemented
- [ ] Gas

### txlog
- [x] Local only
- [x] Local Bypass Check (BYPASS)
- [x] Enforcing blessed hashes
- [x] Guarded db access (Module admin or calledByModule)
- [x] Mock PactDb implementation
- [ ] ChainwebPactDb implementation
- [x] implemented
- [ ] Gas

### update
- [x] Local Bypass Check (NOTBYPASSED)
- [x] Enforcing blessed hashes
- [x] Guarded db access (Module admin or calledByModule)
- [x] Mock PactDb implementation
- [ ] ChainwebPactDb implementation
- [x] implemented
- [ ] Gas

### with-default-read
- [x] Local Bypass Check (BYPASS)
- [x] Enforcing blessed hashes
- [x] Guarded db access (Module admin or calledByModule)
- [x] Mock PactDb implementation
- [ ] ChainwebPactDb implementation
- [x] implemented
- [ ] Gas

### with-read
- [x] Local Bypass Check (BYPASS)
- [x] Enforcing blessed hashes
- [x] Guarded db access (Module admin or calledByModule)
- [x] Mock PactDb implementation
- [ ] ChainwebPactDb implementation
- [x] implemented
- [ ] Gas

### write
- [x] Local Bypass Check (NOTBYPASSED)
- [x] Enforcing blessed hashes
- [x] Guarded db access (Module admin or calledByModule)
- [x] Mock PactDb implementation
- [ ] ChainwebPactDb implementation
- [x] implemented
- [ ] Gas

## Time Builtins

### add-time
- [x] implemented both overloads
- [ ] Gas

### days
- [x] implemented both overloads
- [ ] Gas

### diff-time
- [x] implemented both overloads
- [ ] Gas

### format-time
- [x] implemented
- [ ] Gas

### hours
- [x] implemented both overloads
- [ ] Gas

### minutes
- [x] implemented both overloads
- [ ] Gas

### parse-time
- [x] implemented
- [ ] Gas

### time
- [x] implemented
- [ ] Gas


## Operator Builtins

### !=
- [x] implemented
- [ ] Gas

### &
- [x] implemented
- [ ] Gas

### *
- [x] implemented integer case
- [x] implemented decimal case
- [ ] Gas

### +
- [x] implemented integer case
- [x] implemented decimal case
- [x] implemented string case (concatenation)
- [x] implemented object case (union)
- [ ] Gas

### -
- [x] implement - as negate (1-arg) for integer
- [x] implement - as negate (1-arg) for decimal
- [x] implement - as subtract (2-arg) for integer
- [x] implement - as subtract (2-arg) for decimal
- [x] implemented object case (union)
- [ ] Gas

### <
- [x] implemented integer case
- [x] implemented decimal case
- [x] implemented string case
- [x] implemented time case
- [ ] Gas

### <=
- [x] implemented integer case
- [x] implemented decimal case
- [x] implemented string case
- [x] implemented time case
- [ ] Gas

### =
- [x] implemented integer case
- [x] implemented decimal case
- [x] implemented string case
- [x] implemented bool case
- [x] implemented object case
- [x] implemented time case
- [x] implemented list case
- [x] implemented modref case
- [x] implemented guard case
- [ ] Gas

### >
- [x] implemented integer case
- [x] implemented decimal case
- [x] implemented string case
- [x] implemented time case
- [ ] Gas

### >=
- [x] implemented integer case
- [x] implemented decimal case
- [x] implemented string case
- [x] implemented time case
- [ ] Gas

### ^
- [x] implemented integer case
- [x] implemented decimal case
- [x] Musl
- [ ] Gas

### abs
- [x] implemented integer case
- [x] implemented decimal case
- [ ] Gas

### and
- [x] implemented with lazy eval
- [ ] Gas

### and?
- [x] implemented with lazy eval
- [ ] Gas

### ceiling
- [x] implemented base case
- [x] implemented specific precision case
- [ ] Gas

### dec
- [x] implemented
- [ ] Gas

### exp
- [x] implemented integer case
- [x] implemented decimal case
- [x] Musl
- [ ] Gas

### floor
- [x] implemented base case
- [x] implemented specific precision case
- [ ] Gas

### ln
- [x] implemented integer case
- [x] implemented decimal case
- [x] Musl
- [ ] Gas

### log
- [x] implemented integer case
- [x] implemented decimal case
- [x] Musl
- [ ] Gas

### mod
- [x] implemented integer case
- [ ] Gas

### not
- [x] implemented
- [ ] Gas

### not?
- [x] implemented
- [ ] Gas

### or
- [x] implemented with lazy eval
- [ ] Gas

### or?
- [x] implemented with lazy eval
- [ ] Gas

### round
- [x] implemented base case
- [x] implemented specific precision case
- [ ] Gas

### shift
- [x] implemented
- [ ] Gas

### sqrt
- [x] implemented integer case
- [x] implemented decimal case
- [x] Musl
- [ ] Gas

### xor
- [x] implemented
- [ ] Gas

### | (bitwise or)
- [x] implemented
- [ ] Gas

### | (bitwise not)
- [x] implemented
- [ ] Gas

## Keyset builtins

### define-keyset
- [x] Enforced top level only
- [x] implemented 2-arg case
- [x] implemented 1-arg case
- [x] namespaced
- [ ] Gas

### enforce-keyset
- [x] implemented guard case
- [x] implemented keyset name case
- [ ] Gas

### enforce-keyset
- [x] implemented guard case
- [x] implemented keyset name case
- [ ] Gas

### keys-2, keys-all, keys-any
TODO: do we need this as a native?

### read-keyset
- [x] implemented guard case
- [x] implemented keyset name case
- [ ] Gas
