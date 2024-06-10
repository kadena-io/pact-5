## poseidon-hash-hack-a-chain

Use `poseidon-hash-hack-a-chain` to compute a hash using the Poseidon cryptographic hash function. 
Note: This is a reference version of the Poseidon hash function used by Hack-a-Chain.

### Basic syntax

To compute the hash using the reference version of the Poseidon cryptographic hash function, use the following syntax:

```pact
(poseidon-hash-hack-a-chain input)
```

### Arguments

Use the following arguments to specify the inputs for computing the Poseidon hash using the `poseidon-hash-hack-a-chain` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `input` | integer | Specifies one or more inputs for the Poseidon hash function. |

### Return value

The `poseidon-hash-hack-a-chain` function returns an integer representing the computed Poseidon hash.

### Examples

The following examples demonstrate how to use the `poseidon-hash-hack-a-chain` function with different inputs in the Pact REPL:

```pact
pact> (poseidon-hash-hack-a-chain 1)
18586133768512220936620570745912940619677854269274689475585506675881198879027
pact> (poseidon-hash-hack-a-chain 1 2 3)
6542985608222806190361240322586112750744169038454362455181422643027100751666
pact> (poseidon-hash-hack-a-chain 1 2 3 4 5 6)
20400040500897583745843009878988256314335038853985262692600694741116813247201
pact> (poseidon-hash-hack-a-chain 1 2 3 4 5 6 7 8)
18604317144381847857886385684060986177838410221561136253933256952257712543953
```

In these examples, different sets of inputs are provided to compute the Poseidon hash using the reference version. 
The function returns the computed hash value as an integer.
