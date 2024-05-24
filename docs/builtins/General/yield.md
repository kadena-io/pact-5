## yield
The `yield` function is used to yield an object for use with the 'resume' function in the subsequent Pact step. It optionally allows targeting the subsequent step to execute on a specific chain using automated SPV (Simplified Payment Verification) endorsement-based dispatch.

### Basic syntax

To yield an `OBJECT` for use with `resume`, use the following syntax:

`(yield OBJECT [TARGET-CHAIN])`

### Arguments

Use the following arguments to specify the object and, optionally, the target chain for the subsequent step using the `yield` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `OBJECT` | `object:<{y}>` | Specifies the object to be yielded for use with 'resume'. |
| `TARGET-CHAIN` | `string` | (Optional) Specifies the chain ID on which the subsequent step should execute. |

### Return value

The `yield` function returns the yielded object.

### Examples

The following examples demonstrate the usage of the `yield` function within a Pact script. They yield an object for use with `resume`, optionally targeting the subsequent step to execute on a specific chain:

```pact
(yield { "amount": 100.0 })
```
```pact
(yield { "amount": 100.0 } "some-chain-id")
```

These examples illustrate how to use the `yield` function to pass data between Pact steps and, optionally, direct the subsequent step execution to a specific chain using automated SPV endorsement-based dispatch.
