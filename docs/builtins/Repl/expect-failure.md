## expect-failure

_doc_&nbsp;`string` _exp_&nbsp;`<a>` _&rarr;_&nbsp;`string`

_doc_&nbsp;`string` _err_&nbsp;`string` _exp_&nbsp;`<a>` _&rarr;_&nbsp;`string`

Evaluate EXP and succeed only if it throws an error.

```bash
pact> (expect-failure "Enforce fails on false" (enforce false "Expected error"))
"Expect failure: success: Enforce fails on false"
pact> (expect-failure "Enforce fails with message" "Expected error" (enforce false "Expected error"))
"Expect failure: success: Enforce fails with message"
```
