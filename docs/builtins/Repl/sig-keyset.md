## sig-keyset

Convenience function to build a keyset from keys present in message signatures, using 'keys-all' as the predicate.

### Basic syntax

```pact
(sig-keyset)
```

## Arguments

`sig-keyset` is a 0-argument function.

### Return value

Returns a keyset `guard` constructed from all the signatures present in the tx sigs (Generally set in the repl)/

### Example

The following example demonstrates how to use `sig-keyset` to construct a key present in the tx signatures (REPL Only):

```pact
pact> (env-sigs [{"key":"bob", "caps":[]}])
"Setting transaction signatures/caps"
pact> (sig-keyset)
KeySet {keys: [bob],pred: keys-all}
```
