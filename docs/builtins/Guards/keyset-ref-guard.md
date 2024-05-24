## keyset-ref-guard

Use `keyset-ref-guard` to create a guard for the keyset registered as `KEYSET-REF` with the `define-keyset` function.
Concrete keysets are themselves guard types; this function is specifically to store references alongside other guards in the database, etc.

### Basic syntax

To create a guard for a keyset registered with `define-keyset`, use the following syntax:

`(keyset-ref-guard KEYSET-REF)`

### Arguments

Use the following argument to specify the keyset reference for which you want to create a guard using the `keyset-ref-guard` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `KEYSET-REF` | `string` | Specifies the reference to the keyset registered with `define-keyset`. |

### Return value

The `keyset-ref-guard` function returns a `guard` type corresponding to the specified keyset reference.

### Examples

The following example demonstrates the use of `keyset-ref-guard`:

```pact
(keyset-ref-guard "my-keyset")
```

In this example, `keyset-ref-guard` creates a guard for the keyset registered as "my-keyset" using `define-keyset`.
