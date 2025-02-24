## hash-poseidon

Use `hash-poseidon` to compute the Poseidon Hash Function. 
Note that this is a reference version of the Poseidon hash function.

### Basic syntax

To compute the Poseidon Hash Function using the reference version, use the following syntax:

```pact
(hash-poseidon i j k l m n o p)
```

### Arguments

Use the following arguments to specify the inputs for computing the Poseidon hash using the `hash-poseidon` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `i`, `j`, `k`, `l`, `m`, `n`, `o`, `p` | integer | Specifies the inputs for the Poseidon hash function. |

### Return value

The `hash-poseidon` function returns an integer representing the computed Poseidon hash.

### Examples

The following examples demonstrate the use of `hash-poseidon` in the Pact REPL:

```pact
pact>(hash-poseidon 1)
pact>(hash-poseidon 1 2)
pact>(hash-poseidon 1 2 3 4 5 6)
pact>(hash-poseidon 1 2 3 4 5 6 7 8)
```

In these examples, different sets of inputs are provided to compute the Poseidon hash using the reference version. The function returns the computed hash value as an integer.
