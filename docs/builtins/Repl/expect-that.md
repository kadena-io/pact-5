## expect-that

_doc_&nbsp;`string` _pred_&nbsp;`value:<a> -> bool` _exp_&nbsp;`<a>` _&rarr;_&nbsp;`string`

Evaluate EXP and succeed if value passes predicate PRED.

```bash
pact> (expect-that "addition" (< 2) (+ 1 2))
"Expect-that: success: addition"
pact> (expect-that "addition" (> 2) (+ 1 2))
"FAILURE: addition: did not satisfy (> 2) : 3:integer"
```
