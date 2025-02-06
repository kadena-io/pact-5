## pact-state

Use `pact-state` to inspect state from most the recent defpact step execution. 
This function returns an object with the following fields:

- `pactId`: defpact identifier.
- `yield`: yield result or false if the step didn't produce a yield.
- `step`: executed step number.
- `executed`: indicates if step was skipped because entity did not match.

You can clear the defpact state from the REPL by including the optional `clear` argument set to `true`.

### Basic syntax

To query the current state of the latest defpact execution, use the following syntax:

```pact
(pact-state)
```

To query and clear the defpact state from the REPL, use the following syntax:

```pact
(pact-state true)
```

### Arguments

Use the following argument to clear the most recent defpact step executed from the REPL state:

| Argument | Type | Description |
| --- | --- | --- |
| `clear` | bool | Clear the latest executed defpact from the REPL state (optional). |

### Return value

This function returns an object with the following fields:

- `pactId`: defpact identifier
- `yield`: yield result or false, if the step didn't produce a yield
- `step`: executed step
- `executed`: indicates if step was skipped because entity did not match

### Examples

The following example demonstrates creating a module with a defpact, executing the first step, then querying the execution context using the `pact-state` function:

```pact
pact> (module m g (defcap g () true) (defpact foo () (step (yield {"hello":1})) (step "done!")))
"Loaded module m, hash LkeXOLGPdDYhWfXWngJhQmn6-D0pYNaxGHvP_Nyl6Yg"

pact> (foo)
{"hello": 1}

pact> (pact-state)
{"pactId": "DldRwCblQ7Loqy6wYJnaodHl30d3j3eH-qtFzfEv46g","step": 0,"yield": {"hello": 1}}
```

The following example clears the defpact execution context from the REPL:

```pact
(pact-state true)
{"pactId": "DldRwCblQ7Loqy6wYJnaodHl30d3j3eH-qtFzfEv46g","step": 0,"yield": {"hello": 1}}
```