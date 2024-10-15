## static-redeploy

Use `static-redeploy` to redeploy any module, without _any_ code changes. Roundtripping a legacy module will store the new module in our new, more compact storage format, which will result in less gas on loads.

Note: this leaves governance unchanged.

### Basic syntax

Assume you have some module
```pact
(module m g
  (defcap g () true)

  (defun f () 1)

  (defun gg () 2)
  )
```

To redeploy using the new pact-5, call

```pact
(static-redeploy "m")
```

### Arguments


| Argument | Type | Description
| -------- | ---- | -----------
| `module` | string | Specifies the module to redeploy

### Return values

The unit value `()` will be returned if successful.

### Examples

See: Basic Syntax.
