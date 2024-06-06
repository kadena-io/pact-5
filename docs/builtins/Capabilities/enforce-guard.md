## enforce-guard

Use `enforce-guard` to execute a guard function or a keyset to enforce desired predicate logic.

### Basic syntax

To execute a guard function, use the following syntax:

`(enforce-guard guard)`

To execute a keyset, use the following syntax:

`(enforce-guard keysetname)`

### Arguments

Use the following arguments to specify the guard function or keyset to execute using the `enforce-guard` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `guard` | `guard` | Specifies the guard function to execute. |
| `keysetname` | `string` | Specifies the name of the keyset to execute. |

### Return values

The `enforce-guard` function returns a boolean value indicating whether the guard function or keyset executed successfully.

- If the guard function or keyset executes successfully, `enforce-guard` returns `true`.
- If the guard function or keyset fails to execute, `enforce-guard` returns `false`.

### Examples

The following example executes a keyset named `'admin-keyset'` using `enforce-guard`:

```pact
(enforce-guard 'admin-keyset)
```

The following example executes a guard function named `row-guard` using `enforce-guard`:

```pact
(enforce-guard row-guard)
```
