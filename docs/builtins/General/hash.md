## hash

Use `hash` to compute the BLAKE2b 256-bit hash for a specified `value` of any type.
The resulting hash is an unpadded base64-encoded string that consists of 43 characters from the [`a-zA-Z0-9_-`] character set.
Strings are converted directly.
Other values are converted using their JSON representation. 
Non-value-level arguments are not allowed.

### Basic syntax

To compute the BLAKE2b 256-bit hash of a value, use the following syntax:

```pact
(hash value)
```

### Arguments

Use the following argument to specify the value for the `hash` Pact function:

| Argument | Type | Description  |
|----------|------|--------------|
| `value`  | any | Specifies the value to be hashed. |

### Return values

The `hash` function returns a string representing the computed hash value.

### Examples

The following example demonstrates how to use the `hash` function with a string value:

```pact
pact> (hash "hello")
"Mk3PAn3UowqTLEQfNlol6GsXPe-kuOWJSCU0cbgbcs8"
```

In this example, the `hash` function computes the BLAKE2b 256-bit hash of the string "hello" and returns the hash value.

In the following example, the input value `{ 'foo: 1 }` is a JSON object:

```pact
(hash { 'foo: 1 })
"h9BZgylRf_M4HxcBXr15IcSXXXSz74ZC2IAViGle_z4"
```

The `hash` function computes the BLAKE2b 256-bit hash of the JSON representation of the object and returns the hash value.

The `hash` function is useful for generating hash values of data for various cryptographic operations in Pact contracts.
