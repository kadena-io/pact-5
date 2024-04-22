Use `create-user-guard` to define a custom guard *`CLOSURE`* whose arguments are strictly evaluated at definition time and supplied to the indicated function at enforcement time.

## Basic syntax

To define a custom guard *`CLOSURE`* for use in Pact, use the following syntax:

create-user-guard *`CLOSURE`*

## Arguments

Use the following argument to specify the *`CLOSURE`* for the `create-user-guard` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| closure | closure | Specifies the custom guard closure to define. The closure is a function that takes no arguments and returns a boolean value. |

## Return values

The `create-user-guard` function returns a guard that utilizes the specified custom *`CLOSURE`*.

## Example

The following example demonstrates the `create-user-guard` function:

```lisp
(create-user-guard (read-keyset 'my-keyset))
```

In this example, `(read-keyset 'my-keyset)` is used to obtain a keyset, and this keyset is then used as a custom guard closure in `create-user-guard`. The closure captures the keyset at definition time, and it will be supplied to the indicated function at enforcement time. This allows for custom user-defined guards based on specific keysets or conditions.
