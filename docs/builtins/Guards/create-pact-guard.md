## create-pact-guard

Use `create-pact-guard` to define a predicate function with the specified `name` that captures the results of the `pact-id` function for a `defpact` transaction. 

When enforced, the guard will only return true if the `pact-id` at enforcement is the same as the `pact-id` captured by the `create-pact-guard` function. 
This check ensures that the guard will only succeed within the multi-step transaction identified by the `pact-id`.

### Basic syntax

To define a predicate function by `name` that captures the results of the `pact-id` function, use the following syntax:

```pact
create-pact-guard name
```

### Arguments

Use the following argument to specify the `name` for the `create-pact-guard` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `name` | string | Specifies the name of the predicate function that captures the `pact-id` for a `defpact` multi-step transaction. |

### Return values

The `create-pact-guard` function returns a guard with the specified `name` that captures the `pact-id'` for a `defpact` multi-step transaction.

### Example

The following example demonstrates how to use the `create-pact-guard` function to define a guard named `"pact-id-guard"` that captures the `pact-id'` for a `defpact` multi-step transaction:

```pact
(create-pact-guard "pact-id-guard")
```

This guard ensures that it will only succeed within the multi-transaction identified by the pact id.
