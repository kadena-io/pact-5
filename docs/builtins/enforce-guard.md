Use `enforce-guard` to execute a specified *`GUARD`* or a defined keyset named *`KEYSETNAME`* to enforce the desired predicate logic.

## Basic syntax

To execute a *`GUARD`* or a defined keyset to enforce desired predicate logic, use the following syntax:

enforce-guard *guard* -> bool
enforce-guard *keysetname* -> bool

## Arguments

Use the following arguments to specify the *`GUARD`* or *`KEYSETNAME`* for the `enforce-guard` Pact function:

| Argument   | Type   | Description                                        |
|------------|--------|----------------------------------------------------|
| guard      | guard  | Specifies the guard to execute.                    |
| keysetname | string | Specifies the name of the defined keyset to enforce.|

## Return values

The `enforce-guard` function returns a boolean value indicating whether the guard or keyset enforced the desired predicate logic.

## Examples

The following examples demonstrate the `enforce-guard` function:

1. Execute a guard named 'admin-keyset' to enforce desired logic:

```lisp
(enforce-guard 'admin-keyset)
```

2. Execute a row guard named 'row-guard' to enforce desired logic:

```lisp
(enforce-guard row-guard)
```

In these examples, the `enforce-guard` function is used to execute the specified guard or keyset to enforce the desired predicate logic. The function returns a boolean value indicating whether the guard or keyset enforced the desired logic successfully.
