## resume
The `resume` function is a special form that binds to a yielded object value from the prior step execution in a Pact. If the yield step was executed on a foreign chain, it enforces endorsement via Simple Payment Verification (SPV).

### Basic syntax

To bind to a yielded object value from the prior step execution, use the following syntax:

`(resume BINDING)`

### Arguments

Use the following argument to specify the `BINDING` to be resumed using the `resume` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `BINDING` | `binding:<{r}>` | Specifies the binding representing the yielded object value from the prior step execution. |

### Return value

The `resume` function returns the value bound to the specified `BINDING`.

### Example

The following example demonstrates the usage of the `resume` function within a Pact script. It binds to the yielded object value from the prior step execution:

```lisp
(resume BINDING)
```

This example illustrates how to use the `resume` function to resume the execution from a yielded object value in a Pact script. If the yield step was executed on a foreign chain, it enforces endorsement via SPV.
