## emit-event

Use `emit-event` to emit a specified `CAPABILITY` as an event without evaluating the body of the capability. 
This function fails if the specified `CAPABILITY` doesn't include the `@managed` or `@event` keyword in its declaration.

By convention, capabilities are defined using all uppercase letters.

### Basic syntax

To emit a `CAPABILITY` as an event without evaluating its body, use the following syntax:

```pact
(emit-event CAPABILITY)
```

### Arguments

Use the following argument to specify the `CAPABILITY` for the `emit-event` Pact function.

| Argument   | Type | Description                                       |
|------------|------|---------------------------------------------------|
| `CAPABILITY` | capability | Specifies the capability to emit as an event. |

### Return values

The `emit-event` function returns a boolean value indicating success or failure of emitting the event.

### Examples

The following example demonstrates how to use the `emit-event` function to emit an event for the `TRANSFER` capability with the parameters `"Bob"`, `"Alice"`, and `12.0`:

```pact
pact> (emit-event (TRANSFER "Bob" "Alice" 12.0))
true
```

The function returns a boolean value indicating the success or failure of emitting the event.
