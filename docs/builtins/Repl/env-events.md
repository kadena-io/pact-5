## env-events

_clear_&nbsp;`bool` _&rarr;_&nbsp;`[object:*]`

Retreive any accumulated events and optionally clear event state. Object returned has fields 'name' (fully-qualified event name), 'params' (event parameters), 'module-hash' (hash of emitting module).

```pact
(env-events true)
```
