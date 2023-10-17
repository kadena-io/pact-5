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


