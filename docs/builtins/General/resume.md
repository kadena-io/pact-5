## resume

Use the `resume` function to resume execution of a step in a `defpact`.
This function binds to an object produced by the `yield` function in the prior step of a `pact`. 
If the `yield` function is executed on a different chain, the `resume` function requires a simple payment verification (SPV) proof.

### Basic syntax

To bind to a yielded object produced by a prior step execution, use the following syntax:

```pact
(resume binding)
```

### Arguments

Use the following argument to specify the `binding` to be resumed using the `resume` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `binding` | object | Specifies the binding representing the yielded object from the prior step execution. |

### Return value

The `resume` function returns the value bound to the specified `BINDING`.

### Example

The following example demonstrates how to use the `resume` function in a Pact script to bind to the yielded object `sample` produced by the previous step in the execution of a pact:

```pact
(resume sample)
```
