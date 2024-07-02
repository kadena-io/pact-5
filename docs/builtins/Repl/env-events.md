## env-events

Use `env-events` to retrieve any accumulated events and optionally clear the event state.

### Basic syntax

`(env-events clear)`

### Arguments

Use the following argument to specify whether to clear the event state after retrieving the events.

| Argument | Type | Description |
| --- | --- | --- |
| `clear` | `bool` | Specifies whether to clear the event state after retrieving the events. Set to `true` to clear the event state, or `false` to keep the event state. |

###XS Return value

The `env-events` function returns an array of objects representing the accumulated events. Each object in the array has the following fields:

- `name`: The fully-qualified name of the event.
- `params`: The parameters associated with the event.
- `module-hash`: The hash of the module that emitted the event.
