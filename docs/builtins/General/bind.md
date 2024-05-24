## bind
Use `bind` as a special form to evaluate `src` to an object which is then bound to with `bindings` over subsequent body statements.

### Basic syntax

To evaluate `src` to an object and bind it with `bindings` over subsequent body statements, use the following syntax:

`(bind src bindings)`

### Arguments

Use the following arguments to specify the *`src`* object and *`bindings`* for the `bind` special form.

| Argument | Type | Description |
| --- | --- | --- |
| `src` | `object:<{row}>` | Specifies the source object to evaluate. |
| `bindings` | `binding:<{row}>` | Specifies the bindings to apply to the `src` object. |

### Return values

The `bind` special form returns the result of evaluating the *`src`* object with the provided *`bindings`*.

### Examples

The following example demonstrates the `bind` special form in the Pact REPL:

```pact
pact>(bind { "a": 1, "b": 2 } { "a" := a-value } a-value)
1
```

In this example, `bind` evaluates the object `{ "a": 1, "b": 2 }` and binds the value of "a" to `a-value`. It then returns the value `1`, which is the value bound to `a-value`.
