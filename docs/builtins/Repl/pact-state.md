## pact-state

Inspect state from most recent pact execution. Returns object with fields
  'pactId': pact ID; 'yield': yield result or 'false' if none; 'step': executed
  step; 'executed': indicates if step was skipped because entity did not match.
  With CLEAR argument, erases pact from repl state.

### Basic syntax

To query the current state of the latest defpact execution

```pact
(pact-state)
```

To query state and also clear it, use

```pact
(pact-state true)
```

### Arguments

Use the following argument to query the latest executed defpact's state:

| Argument | Type | Description |
| --- | --- | --- |
| `clear` | bool | Clear the latest executed defpact from the repl state (optional) |


### Return value

Returns Returns object with fields
  'pactId': pact ID; 'yield': yield result or 'false' if none; 'step': executed
  step; 'executed': indicates if step was skipped because entity did not match.

### Examples


```pact
pact> (module m g (defcap g () true) (defpact foo () (step (yield {"hello":1})) (step "done!")))
"Loaded module m, hash LkeXOLGPdDYhWfXWngJhQmn6-D0pYNaxGHvP_Nyl6Yg"
pact> (foo)
{"hello": 1}
pact> (pact-state)
{"executed": true
,"pactId": "DldRwCblQ7Loqy6wYJnaodHl30d3j3eH-qtFzfEv46g"
,"step": 0
,"yield": {"hello": 1}}
```
