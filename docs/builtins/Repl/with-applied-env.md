## with-applied-env

_exec_&nbsp;`<a>` _&rarr;_&nbsp;`<a>`

Evaluate EXEC with any pending environment changes applied. Normally, environment changes must execute at top-level for the change to take effect. This allows scoped application of non-toplevel environment changes.

```bash
pact> (let ((a 1)) (env-data { 'b: 1 }) (with-applied-env (+ a (read-integer 'b))))
2
```
