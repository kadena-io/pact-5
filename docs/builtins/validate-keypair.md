The `validate-keypair` function enforces that the Curve25519 keypair composed of the public key (PUBLIC) and the private key (SECRET) provided as base-16 strings of length 32 match each other.

### Basic syntax

To validate a Curve25519 keypair, use the following syntax:

validate-keypair *public* *secret*

### Arguments

Use the following arguments to specify the public key and the private key for validation using the `validate-keypair` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| public | string | Specifies the public key as a base-16 string of length 32. |
| secret | string | Specifies the private key as a base-16 string of length 32. |

### Return value

The `validate-keypair` function returns a boolean value indicating whether the provided keypair matches.

### Examples

The following example demonstrates the usage of the `validate-keypair` function within a Pact script. It validates that the Curve25519 keypair composed of the public key `pubkey` and the private key `privkey` match each other:

```lisp
(validate-keypair pubkey privkey)
```

This example illustrates how to use the `validate-keypair` function to ensure that a given public and private key pair match in Pact, verifying their integrity.
