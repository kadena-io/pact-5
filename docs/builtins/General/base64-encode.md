## base64-encode

Use `base64-encode` to convert the specified `string` to an unpadded base64-encoded string.

### Basic syntax

To encode a `string` as unpadded base64, use the following syntax:

```pact
(base64-encode string)
```

### Arguments

Use the following argument to specify the `string` to encode using the `base64-encode` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `string` | string | Specifies the `string` to encode as unpadded base64. |

### Return values

The `base64-encode` function returns the unpadded base64 encoded `string`.

### Examples

The following example converts the "hello world!" into the unpadded base64-encoded string "aGVsbG8gd29ybGQh" in the Pact REPL:

```pact
pact> (base64-encode "hello world!")
"aGVsbG8gd29ybGQh"
```

In this example, `base64-encode` encodes t
