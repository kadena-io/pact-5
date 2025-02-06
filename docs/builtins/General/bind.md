## bind

Use `bind` to evaluate a `src` object, then apply the specified `bindings` to bind field variables to values over subsequent body statements.

### Basic syntax

To evaluate `src` to an object and bind it with `bindings` over subsequent body statements, use the following syntax:

```pact
(bind src bindings)
```

### Arguments

Use the following arguments to specify the `src` object and `bindings` for the `bind` special form.

| Argument | Type | Description |
| --- | --- | --- |
| `src` | `object:{row}` | Specifies the source object to evaluate. |
| `bindings` | `binding:{row}` | Specifies the bindings to apply to the `src` object. |

### Return value

The `bind` returns the result of evaluating the `src` object with the specifies `bindings`.
The data type depends on the data type of the field you specify for the `bindings` argument.

### Examples

The following example demonstrates the `bind` special form in the Pact REPL:

```pact
pact> (bind { "a": 1, "b": 2 } { "a" := a-value } a-value)
1
```

In this example, `bind` evaluates the object `{ "a": 1, "b": 2 }` and binds the value of "a" to `a-value`. It then returns the value `1`, which is the value bound to `a-value`.
