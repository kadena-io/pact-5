Use `describe-table` to get metadata for a specified *`TABLE`*. This function returns an object with fields including 'name', 'hash', 'blessed', 'code', and 'keyset'.

## Basic syntax

To get metadata for a *`TABLE`*, use the following syntax:

describe-table *`TABLE`*

## Arguments

Use the following argument to specify the *`TABLE`* for the `describe-table` Pact function.

| Argument | Type          | Description                                  |
|----------|---------------|----------------------------------------------|
| table    | table:<{row}> | Specifies the table to describe.             |

## Return values

The `describe-table` function returns an object with metadata for the specified *`TABLE`*.

## Examples

The following example demonstrates the `describe-table` function:

```lisp
(describe-table accounts)
```

In this example, `(describe-table accounts)` is used to get metadata for the table named 'accounts'. The function returns an object with fields such as 'name', 'hash', 'blessed', 'code', and 'keyset', providing detailed information about the table.
