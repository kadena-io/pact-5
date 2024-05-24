## poseidon-hash-hack-a-chain
Use `poseidon-hash-hack-a-chain` to compute the Poseidon Hash Function. Note: This is a reference version of the Poseidon hash function used by Hack-a-Chain.

### Basic syntax

To compute the Poseidon Hash Function using the reference version, use the following syntax:

poseidon-hash-hack-a-chain *i j k l m n o p*

### Arguments

Use the following arguments to specify the inputs for computing the Poseidon hash using the `poseidon-hash-hack-a-chain` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| i, j, k, l, m, n, o, p | integer | Specifies the inputs for the Poseidon hash function. |

### Return value

The `poseidon-hash-hack-a-chain` function returns an integer representing the computed Poseidon hash.

### Examples

The following examples demonstrate the use of `poseidon-hash-hack-a-chain` in the Pact REPL:

```pact
pact>(poseidon-hash-hack-a-chain 1)
pact>(poseidon-hash-hack-a-chain 1 2)
pact>(poseidon-hash-hack-a-chain 1 2 3 4 5 6)
pact>(poseidon-hash-hack-a-chain 1 2 3 4 5 6 7 8)
```

In these examples, different sets of inputs are provided to compute the Poseidon hash using the reference version. The function returns the computed hash value as an integer.
