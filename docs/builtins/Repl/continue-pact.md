## continue-pact

_step_&nbsp;`integer` _&rarr;_&nbsp;`string`

_step_&nbsp;`integer` _rollback_&nbsp;`bool` _&rarr;_&nbsp;`string`

_step_&nbsp;`integer` _rollback_&nbsp;`bool` _pact-id_&nbsp;`string` _&rarr;_&nbsp;`string`

_step_&nbsp;`integer` _rollback_&nbsp;`bool` _pact-id_&nbsp;`string` _yielded_&nbsp;`object:<{y}>` _&rarr;_&nbsp;`string`

Continue previously-initiated pact identified STEP, optionally specifying ROLLBACK (default is false), PACT-ID of the pact to be continued (defaults to the pact initiated in the current transaction, if one is present), and YIELDED value to be read with 'resume' (if not specified, uses yield in most recent pact exec, if any).

```pact
(continue-pact 1)
(continue-pact 1 true)
(continue-pact 1 false "[pact-id-hash]"))
(continue-pact 2 1 false "[pact-id-hash]" { "rate": 0.9 })
```
