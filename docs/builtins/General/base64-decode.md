## base64-decode

Use `base64-decode` to convert a previously-encoded `string` from unpadded base64 encoding to a string.

### Basic syntax

To decode a `string` from unpadded base64 encoding, use the following syntax:

```pact
(base64-decode string)
```

### Arguments

Use the following argument to specify the `string` to decode using the `base64-decode` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `string` | string | Specifies the base64-encoded `string` to decode. |

### Return value

The `base64-decode` function returns the decoded `string`.

### Examples

The following example decodes a base64-encoded "aGVsbG8gd29ybGQh" string to the decoded "hello world!" string in the Pact REPL:

```pact
pact> (base64-decode "aGVsbG8gd29ybGQh")
"hello world!"
```
