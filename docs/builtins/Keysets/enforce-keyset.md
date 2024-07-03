## enforce-keyset

Use `enforce-keyset` to execute a specified `guard` or a defined keyset named `keysetname` to enforce the desired predicate logic.

### Basic syntax

To execute a `guard` to enforce desired predicate logic, use the following syntax:

```pact
(enforce-keyset guard)
```

To require a specified keyset to enforce desired predicate logic, use the following syntax:

```pact
(enforce-keyset keysetname)
```

### Arguments

Use the following arguments to specify the `guard` or `keysetname` for the `enforce-keyset` Pact function:

| Argument | Type | Description |
|----------|------|------------ |
| `guard` | guard | Specifies the guard to execute. |
| `keysetname` | string | Specifies the name of the defined keyset to enforce. |

### Return values

The `enforce-keyset` function returns a boolean value indicating whether the specified guard or keyset predicate logic was enforced.

### Examples

The following example demonstrates how to use the `enforce-keyset` function to enforce logic defined in the 'admin-keyset' predicate function:

```pact
(enforce-keyset 'admin-keyset)
```

If the condition specified by the `admin-keyset` predicate function is satisfied, the `enforce-keyset` function returns a boolean value of true.

The following example enforces the logic defined in the 'row-guard' predicate logic:

```pact
(enforce-keyset row-guard)
```

If the condition specified by the `row-guard` predicate function is satisfied, the `enforce-keyset` function returns a boolean value of true.
