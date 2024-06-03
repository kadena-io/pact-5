## describe-module

_module_&nbsp;`string` _&rarr;_&nbsp;`object:*`

Get metadata for MODULE. Returns an object with 'name', 'hash', 'blessed',
'code', and 'keyset' fields.

```pact
(describe-module 'my-module)
```

Top level only: this function will fail if used in module code.
