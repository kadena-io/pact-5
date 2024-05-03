## enforce-keyset
Use `enforce-keyset` to execute a specified *`GUARD`* or a defined keyset named *`KEYSETNAME`* to enforce the desired predicate logic.

### Basic syntax

To execute a *`GUARD`* or a defined keyset to enforce desired predicate logic, use the following syntax:

enforce-keyset *guard* -> bool
enforce-keyset *keysetname* -> bool

### Arguments

Use the following arguments to specify the *`GUARD`* or *`KEYSETNAME`* for the `enforce-keyset` Pact function:

| Argument   | Type   | Description                                        |
|------------|--------|----------------------------------------------------|
| guard      | guard  | Specifies the guard to execute.                    |
| keysetname | string | Specifies the name of the defined keyset to enforce.|

### Return values

The `enforce-keyset` function returns a boolean value indicating whether the guard or keyset enforced the desired predicate logic.

### Examples

The following examples demonstrate the `enforce-keyset` function:

1. Execute a guard named 'admin-keyset' to enforce desired logic:

```lisp
(enforce-keyset 'admin-keyset)
```

2. Execute a row guard named 'row-guard' to enforce desired logic:

```lisp
(enforce-keyset row-guard)
```

In these examples, the `enforce-keyset` function is used to execute the specified guard or keyset to enforce the desired predicate logic. The function returns a boolean value indicating whether the guard or keyset enforced the desired logic successfully.
