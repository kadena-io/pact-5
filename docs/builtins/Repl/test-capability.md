## test-capability

_capability_&nbsp;` -> bool` _&rarr;_&nbsp;`string`

Acquire (if unmanaged) or install (if managed) CAPABILITY. CAPABILITY and any composed capabilities are in scope for the rest of the transaction.

```pact
(test-capability (MY-CAP))
```
