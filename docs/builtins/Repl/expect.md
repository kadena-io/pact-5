## expect

_doc_&nbsp;`string` _expected_&nbsp;`<a>` _actual_&nbsp;`<a>` _&rarr;_&nbsp;`string`

Evaluate ACTUAL and verify that it equals EXPECTED.

```bash
pact> (expect "Sanity prevails." 4 (+ 2 2))
"Expect: success: Sanity prevails."
```
