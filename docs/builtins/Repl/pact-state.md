## pact-state

_&rarr;_&nbsp;`object:*`

_clear_&nbsp;`bool` _&rarr;_&nbsp;`object:*`

Inspect state from most recent pact execution. Returns object with fields 'pactId': pact ID; 'yield': yield result or 'false' if none; 'step': executed step; 'executed': indicates if step was skipped because entity did not match. With CLEAR argument, erases pact from repl state.

```pact
(pact-state)
(pact-state true)
```
