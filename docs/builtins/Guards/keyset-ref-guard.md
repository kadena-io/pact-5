## keyset-ref-guard

Use `keyset-ref-guard` to create a guard for the keyset registered as `keyset-ref` using the `define-keyset` function.
Concrete keysets are themselves guard types.
This function is specifically to store references alongside other guards in the database.

### Basic syntax

To create a guard for a keyset registered with the `define-keyset` function, use the following syntax:

```pact
(keyset-ref-guard keyset-ref)
```

### Arguments

Use the following argument to specify the keyset reference for which you want to create a guard using the `keyset-ref-guard` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `keyset-ref` | string | Specifies the reference to the keyset registered with the `define-keyset` function. |

### Return value

The `keyset-ref-guard` function returns a `guard` type corresponding to the specified keyset reference.

### Examples

The following example demonstrates how to use the `keyset-ref-guard` function to create a guard for the keyset registered as "my-keyset" using the `define-keyset` function:

```pact
(keyset-ref-guard "my-keyset")
```
