Use `base64-encode` to encode a *`string`* as unpadded base64.

### Basic syntax

To encode a *`string`* as unpadded base64, use the following syntax:

base64-encode *string*

### Arguments

Use the following argument to specify the *`string`* to encode using the `base64-encode` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| string | string | Specifies the *`string`* to encode as unpadded base64. |

### Return values

The `base64-encode` function returns the unpadded base64 encoded *`string`*.

### Examples

The following example encodes a string as unpadded base64 in the Pact REPL:

```lisp
pact>(base64-encode "hello world!")
"aGVsbG8gd29ybGQh"
```

In this example, `base64-encode` encodes the string "hello world!" as unpadded base64, resulting in the encoded string "aGVsbG8gd29ybGQh".
