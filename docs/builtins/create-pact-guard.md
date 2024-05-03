## create-pact-guard
Use `create-pact-guard` to define a guard predicate by *`NAME`* that captures the results of 'pact-id'. At enforcement time, the success condition is that at that time 'pact-id' must return the same value. This ensures that the guard will only succeed within the multi-transaction identified by the pact id.

### Basic syntax

To define a guard predicate by *`NAME`* that captures the results of 'pact-id', use the following syntax:

create-pact-guard *`NAME`*

### Arguments

Use the following argument to specify the *`NAME`* for the `create-pact-guard` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| name | string | Specifies the name of the guard to create, capturing the 'pact-id' results. |

### Return values

The `create-pact-guard` function returns a guard predicate identified by the specified *`NAME`*, which captures the 'pact-id' results.

### Example

The following example demonstrates the `create-pact-guard` function:

```lisp
(create-pact-guard "pact-id-guard")
```

In this example, `(create-pact-guard "pact-id-guard")` is used to define a guard named `"pact-id-guard"` that captures the results of 'pact-id'. This guard ensures that it will only succeed within the multi-transaction identified by the pact id.
