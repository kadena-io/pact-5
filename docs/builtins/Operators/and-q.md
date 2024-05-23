## and?
Use `and?` to apply a logical AND operation to the results of applying a `value` to `a` and `b`, with short-circuiting.

### Basic syntax

To apply logical 'and' to the results of applying `value` to `a` and `b`, use the following syntax:

`(and? a b value)`

### Arguments

Use the following arguments to specify the functions and `value` for the `and?` operation.

| Argument | Type | Description |
| --- | --- | --- |
| `a` | `x:<r> -> bool` | Specifies the first function to apply `value` to. |
| `b` | `x:<r> -> bool` | Specifies the second function to apply `value` to. |
| `value` | `<r>` | Specifies the value to apply to both `a` and `b` functions. |

### Return values

The `and?` function returns a boolean value based on the result of applying `value` to `a` and `b` with the logical AND operation.

### Examples

The following example demonstrates the `and?` operation in the Pact REPL:

```lisp
pact>(and? (> 20) (> 10) 15)
true
```

In this example, the `and?` function applies the functions `> 20` and `> 10` to the value `15`, resulting in `false` because the second conditions `10 > 15` is false.
