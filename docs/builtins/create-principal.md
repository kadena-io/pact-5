Use `create-principal` to create a principal that unambiguously identifies a specified *`GUARD`*.

## Basic syntax

To create a principal that identifies a *`GUARD`*, use the following syntax:

create-principal *`GUARD`*

## Arguments

Use the following argument to specify the *`GUARD`* for the `create-principal` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| guard | guard | Specifies the guard for which to create a principal. |

## Return values

The `create-principal` function returns a string representing a principal that unambiguously identifies the specified *`GUARD`*.

## Example

The following example demonstrates the `create-principal` function:

```lisp
(create-principal (read-keyset 'keyset))
```

In this example, `(create-principal (read-keyset 'keyset))` is used to create a principal that unambiguously identifies the `keyset` guard. This principal can then be used for various purposes such as access control in Pact code.
