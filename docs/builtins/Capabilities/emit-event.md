## emit-event
Use `emit-event` to emit a specified `CAPABILITY` as an event without evaluating the body of the capability. This function fails if the `CAPABILITY` is not marked as `@managed` or `@event`.

### Basic syntax

To emit a `CAPABILITY` as an event without evaluating its body, use the following syntax:

`(emit-event CAPABILITY)`

### Arguments

Use the following argument to specify the `CAPABILITY` for the `emit-event` Pact function.

| Argument   | Type | Description                                       |
|------------|------|---------------------------------------------------|
| `capability` | `capability`     | Specifies the capability to emit as an event.      |

### Return values

The `emit-event` function returns a boolean value indicating success or failure of emitting the event.

### Examples

The following example demonstrates the `emit-event` function:

```lisp
pact>(emit-event (TRANSFER "Bob" "Alice" 12.0))
true
```

In this example, `(emit-event (TRANSFER "Bob" "Alice" 12.0))` is used to emit the capability `TRANSFER` with parameters `"Bob"`, `"Alice"`, and `12.0` as an event. The function returns a boolean value indicating the success or failure of emitting the event.
