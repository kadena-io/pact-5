## sig-keyset

Use `sig-keyset` as a convenience to build a keyset from keys present in message signatures, using 'keys-all' as the predicate.

### Basic syntax

To build a keyset from keys present in message signatures, use the following syntax:

```pact
(sig-keyset)
```

## Arguments

The `sig-keyset` function does not take any arguments.

### Return value

The `sig-keyset` function returns a keyset `guard` constructed from all of the signatures specified in the transaction environment data. 
In most cases, signatures are defined in `.repl` files or when using the REPL interactively using the `env-sigs` function.

### Example

The following example demonstrates how to use `sig-keyset` to construct a keyset guard from the signatures defined for a transaction:

```pact
pact> (env-sigs [{"key":"bob", "caps":[]}])
"Setting transaction signatures/caps"

pact> (sig-keyset)
KeySet {keys: [bob],pred: keys-all}
```
