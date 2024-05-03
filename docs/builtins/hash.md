## hash
Use `hash` to compute the BLAKE2b 256-bit hash of a `VALUE`, represented in unpadded base64-url. Strings are converted directly, while other values are converted using their JSON representation. Non-value-level arguments are not allowed.

### Basic syntax

To compute the BLAKE2b 256-bit hash of a value, use the following syntax:

hash *value* -> *result*

### Arguments

Use the following argument to specify the value for the `hash` Pact function:

| Argument  | Type   | Description                                       |
|-----------|--------|---------------------------------------------------|
| value     | <a>    | Specifies the value to be hashed.                 |

### Return values

The `hash` function returns a string representing the computed hash value.

### Examples

The following examples demonstrate the `hash` function:

```lisp
(hash "hello")
```

In this example, `"hello"` is a string value. The `hash` function computes the BLAKE2b 256-bit hash of the string "hello" and returns the hash value.

```lisp
(hash { 'foo: 1 })
```

In this example, `{ 'foo: 1 }` is a JSON object. The `hash` function computes the BLAKE2b 256-bit hash of the JSON representation of the object and returns the hash value.

The `hash` function is useful for generating hash values of data for various cryptographic operations in Pact contracts.
