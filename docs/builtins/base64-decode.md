Use `base64-decode` to decode a *`string`* from unpadded base64 encoding.

## Basic syntax

To decode a *`string`* from unpadded base64 encoding, use the following syntax:

base64-decode *string*

## Arguments

Use the following argument to specify the *`string`* to decode using the `base64-decode` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| string | string | Specifies the base64 encoded *`string`* to decode. |

## Return values

The `base64-decode` function returns the decoded *`string`*.

## Examples

The following example decodes a base64 encoded string in the Pact REPL:

```lisp
pact>(base64-decode "aGVsbG8gd29ybGQh")
"hello world!"
```

In this example, `base64-decode` decodes the base64 encoded string "aGVsbG8gd29ybGQh" to the original string "hello world!".
