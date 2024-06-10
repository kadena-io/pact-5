## enforce-guard

Use `enforce-guard` to execute a specified `guard` or a defined keyset named `keysetname` to enforce the predicate logic.

### Basic syntax

To use a `guard` to enforce specific conditions defined in predicate logic, use the following syntax:

```pact
(enforce-guard guard)
```

To use a defined keyset as a `guard` to enforce specific conditions defined in predicate logic, use the following syntax:

```pact
(enforce-guard keysetname)
```

### Arguments

Use the following arguments to specify the `guard` or `keysetname` for the `enforce-guard` Pact function:

| Argument   | Type   | Description                                        |
|------------|--------|----------------------------------------------------|
| `guard` | guard | Specifies the name of the guard to execute. |
| `keysetname` | string | Specifies the name of the defined keyset to enforce.|

### Return values

The `enforce-guard` function returns a boolean value indicating whether the conditions specified in the predicate logic that the guard or keyset is there to enforce were met.

### Examples

The following example demonstrates using the keyset guard named `admin-keyset` to enforce specific signing requirements defined in the keyset predicate function, for example with the `keys-all` or `keys-2`predicate:

```pact
(enforce-guard 'admin-keyset)
```

In most cases, you use `enforce-guard` in the context of contract logic to ensure a specific condition is `true` before allowing a specific operation to be executed. 
For example, you might have logic to validate a sender account before allowing a transfer operation.
You can then call the `enforce-guard` to ensure the sender meets the specified conditions—returning `true` as a result—before executing further logic:

```pact
(enforce-guard sender-guard)
```
