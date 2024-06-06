## continue

Use `continue` to continue a previously-started multi-step transaction.
Transactions that have multiple steps executed in a sequence are called pacts and are defined using the `defpact` keyword.
Steps can be nested in `defpact` structures and the `continue` function enables you to continue execution with a specified value.

### Basic syntax

To continue a previously-started `defpact` transaction, use the following syntax:

```pact
(continue value)
```

### Arguments

Use the following argument to specify the `value` to continue the nested `defpact`.

| Argument | Type | Description |
| --- | --- | --- |
| `value` | `*` | Specifies the value to continue the nested `defpact`. |

### Return values

The `continue` function continues the execution of the nested `defpact` with the specified `value`.

### Examples

The following example demonstrates the use of `continue` to continue a nested `defpact`:

```pact
(continue f)
```

In this example, `(continue f)` is used to continue the execution of the nested `defpact` with the value `f`. This would be used within the context of a `defpact` to resume its execution with a specified value.
