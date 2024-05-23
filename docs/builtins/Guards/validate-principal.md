## validate-principal
The `validate-principal` function validates that a principal unambiguously identifies a specified guard.

### Basic syntax

To validate a `principal` against `GUARD`, use the following syntax:

`(validate-principal GUARD PRINCIPAL)`

### Arguments

Use the following arguments to specify the guard and the principal for validation using the `validate-principal` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `GUARD` | `guard` | Specifies the guard to validate against. |
| `PRINCIPAL` | `string` | Specifies the principal to be validated. |

### Return value

The `validate-principal` function returns a `boolean` value indicating whether the provided principal unambiguously identifies the specified guard.

### Examples

The following example demonstrates the usage of the `validate-principal` function within a Pact script. It enforces that the principal obtained from reading a keyset matches the specified account, ensuring it is valid:

```lisp
(enforce (validate-principal (read-keyset 'keyset) account) "Invalid account ID")
```

This example illustrates how to use the `validate-principal` function to validate that a principal unambiguously identifies a specified guard in Pact, ensuring the security and integrity of the identification process.
