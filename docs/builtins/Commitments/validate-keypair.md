## validate-keypair

Use `validate-keypair` to verify that the Curve25519 `public` key and `secret` key you specify are base-16 strings of 32 characters match each other.

### Basic syntax

To validate a Curve25519 public and secret key pair, use the following syntax:

```pact
(validate-keypair public secret)
```

### Arguments

Use the following arguments to specify the public key and the secret key that you want to validate using the `validate-keypair` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `public` | string | Specifies the public key as a base-16 string with a length of 32 characters. |
| `secret` | string | Specifies the secret key as a base-16 string with a length of 32 characters. |

### Return value

The `validate-keypair` function returns a boolean value indicating whether the provided key pair matches.

### Examples

The following example demonstrates how to use the `validate-keypair` function to verify that the Curve25519 key pair composed of the public key `public-key` and the secret key `secret-key` match:

```pact
(validate-keypair public-key secret-key)
true
```
