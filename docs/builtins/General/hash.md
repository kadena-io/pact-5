## hash

Use `hash` to compute the BLAKE2b 256-bit hash of a specified `value`. The resulting hash value is a Base64Url-encoded string without padding. 
Strings values are converted directly.
Other data type values are converted using their JSON representation. Non-value-level arguments are not allowed.

By convention, the data type `<a>` is used to represent a type-bound parameter like the `value` argument in this function.

### Basic syntax

To compute the BLAKE2b 256-bit hash of a value, use the following syntax:

```pact
(hash value)
```

### Arguments

Use the following argument to specify the value for the `hash` Pact function:

| Argument  | Type   | Description |
|-----------|--------|-------------|
| `value` | `<a>` | Specifies the value to be hashed. |

### Return value

The `hash` function returns a string representing the computed hash value.

### Examples

The following example demonstrates how to use the `hash` function to compute a hash for the `"hello"` string value:

```pact
pact> (hash "hello")
"Mk3PAn3UowqTLEQfNlol6GsXPe-kuOWJSCU0cbgbcs8"
```

Because `"hello"` is a string value, the `hash` function computes the BLAKE2b 256-bit hash of the string "hello" and returns the hash value.

```pact
(hash { 'foo: 1 })
"h9BZgylRf_M4HxcBXr15IcSXXXSz74ZC2IAViGle_z4"
```

In this example, `{ 'foo: 1 }` is a JSON object. The `hash` function computes the BLAKE2b 256-bit hash of the JSON representation of the object and returns the hash value.

The `hash` function is useful for generating hash values of data for various cryptographic operations in Pact contracts.
