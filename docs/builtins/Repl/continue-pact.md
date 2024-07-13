## continue-pact

Use `continue-pact` to continue a previously-initiated multi-step transaction defined using a `defpact` declaration.
You must specify the `step` at which to continue.
You can also specify optional parameters for rollback, pact ID, and yielded value.

### Basic syntax

To continue a pact with the specified step, use the following syntax:

```pact
(continue-pact step)
```

To continue a pact with the specified step and rollback option, use the following syntax:

```pact
(continue-pact step rollback)
```

To continue a pact with the specified step, rollback option, and pact ID, use the following syntax:

```pact
(continue-pact step rollback pact-id)
```

To continue a pact with the specified step, rollback option, pact ID, and yielded value, use the following syntax:

```pact
(continue-pact step rollback pact-id yielded)
```

## Arguments

Use the following arguments to customize the behavior of the `continue-pact` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `step` | integer | Specifies the step of the pact to continue. |
| `rollback` | bool | Specifies whether to perform a rollback (optional). The default is `false`. |
| `pact-id` | string | Specifies the identifier of the pact to continue (optional). The default is the pact identifier initiated in the current transaction, if one is present. |
| `yielded` | object | Specifies the yielded value to be read with the `resume` function (optional). If not specified, the function uses the yield from the most recent pact execution, if any. The schema of the yielded object is `object:<{y}>`. |

## Return value

The `continue-pact` function returns a string indicating the result of continuing the pact.

## Examples

The following example demonstrates how to use the `continue-pact` function to continue a pact with step 1:

```pact
(continue-pact 1)
```

The following example demonstrates how to use the `continue-pact` function to continue a pact with step 1 and perform a rollback:

```pact
(continue-pact 1 true)
```
The following example demonstrates how to use the `continue-pact` function to continue a pact with step 1, without rollback, and specify the pact ID:

```pact
(continue-pact 1 false "[pact-id-hash]")
```

The following example demonstrates how to use the `continue-pact` function to continue a pact with step 2, without rollback, specify the pact ID, and provide a yielded value:

```pact
(continue-pact 2 false "[pact-id-hash]" { "rate": 0.9 })
```
