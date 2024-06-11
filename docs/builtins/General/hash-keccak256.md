## hash-keccak256

Use `hash-keccak256` to compute the hash of a list of unpadded base64url-encoded input `values` using the Keccak-256 cryptographic hash function. 
The hash is computed incrementally over all of the decoded inputs.

### Basic syntax

To compute the Keccak-256 hash for a list of inputs, use the following syntax:

`(hash-keccak256 values)`

### Arguments

Use the following argument to specify the list of inputs for the `hash-keccak256` Pact function:

| Argument | Type | Description |
|----------|------|-------------|
| `values`  | [string] | Specifies the list of unpadded base64url-encoded input values. |

### Return values

The `hash-keccak256` function returns a string representing the computed hash value.

### Examples

The following example demonstrates how to use the `hash-keccak256` function with an empty list `[]` provided as input:

```pact
pact>(hash-keccak256 [])
"xdJGAYb3IzySfn2y3McDwOUAtlPKgic7e_rYBF2FpHA"
```

In this example, the `hash-keccak256` function computes the hash of the empty list and returns the hash value.

In the following example, the list of values contains multiple base64url-encoded strings as input: 

```pact (hash-keccak256 ["c3BpcmVrZXk" "Z3JhcGhxbA" "aGVsbG8gd29ybGQh"])
```

The `hash-keccak256` function computes the hash of all the inputs and returns the hash value.
