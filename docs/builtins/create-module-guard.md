Use `create-module-guard` to define a guard by *`NAME`* that enforces the current module admin predicate.

## Basic syntax

To define a guard by *`NAME`* that enforces the current module admin predicate, use the following syntax:

create-module-guard *`NAME`*

## Arguments

Use the following argument to specify the *`NAME`* for the `create-module-guard` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| name | string | Specifies the name of the guard to create, enforcing the current module admin predicate. |

## Return values

The `create-module-guard` function returns a guard that enforces the current module admin predicate, identified by the specified *`NAME`*.

## Example

The following example demonstrates the `create-module-guard` function:

```lisp
(create-module-guard "module-admin-guard")
```

In this example, `(create-module-guard "module-admin-guard")` is used to define a guard named `"module-admin-guard"` that enforces the current module admin predicate. This guard can then be used to enforce module admin privileges in Pact code.
