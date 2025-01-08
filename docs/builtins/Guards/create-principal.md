## create-principal

Use `create-principal` to create a principal account that unambiguously identifies a specified `guard` predicate function.

For an introduction to principal accounts, see [Accounts, keys, and principals](/smart-contracts/accounts).

### Basic syntax

To create a principal that identifies a `guard` predicate function, use the following syntax:

```pact
create-principal guard
```

### Arguments

Use the following argument to specify the `guard` for the `create-principal` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `guard` | guard | Specifies the guard for which to create a principal. |

### Return values

The `create-principal` function returns a string representing a principal that unambiguously identifies the specified `guard` predicate function.

### Example

The following example demonstrates how to use the `create-principal` function to create a principal that unambiguously identifies the `keyset` guard:

```pact
(create-principal (read-keyset 'keyset))
```

This principal can then be used for various purposes such as access control in Pact code.
