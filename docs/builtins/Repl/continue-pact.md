## continue-pact

Use `continue-pact` to continue a previously-initiated pact identified by a specific step, with optional parameters for rollback, pact ID, and yielded value.

### Basic syntax

To continue a pact with the specified step, use the following syntax:

```lisp
(continue-pact step)
```

To continue a pact with the specified step and rollback option, use the following syntax:

```lisp
(continue-pact step rollback)
```

To continue a pact with the specified step, rollback option, and pact ID, use the following syntax:

```lisp
(continue-pact step rollback pact-id)
```

To continue a pact with the specified step, rollback option, pact ID, and yielded value, use the following syntax:

```lisp
(continue-pact step rollback pact-id yielded)
```

## Arguments

Use the following arguments to customize the behavior of the `continue-pact` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `step` | `integer` | Specifies the step of the pact to continue. |
| `rollback` | `bool` | (Optional) Specifies whether to perform a rollback. Default is `false`. |
| `pact-id` | `string` | (Optional) Specifies the ID of the pact to continue. Defaults to the pact initiated in the current transaction, if one is present. |
| `yielded` | `object` | (Optional) Specifies the yielded value to be read with 'resume'. If not specified, uses the yield from the most recent pact execution, if any. The schema of the yielded object is `object:<{y}>`. |

## Return value

The `continue-pact` function returns a string indicating the result of continuing the pact.

## Examples

The following examples demonstrate the usage of the `continue-pact` function:

1. Continue a pact with step 1:

```pact
(continue-pact 1)
```

2. Continue a pact with step 1 and perform a rollback:

```pact
(continue-pact 1 true)
```

3. Continue a pact with step 1, without rollback, and specify the pact ID:

```pact
(continue-pact 1 false "[pact-id-hash]")
```

4. Continue a pact with step 2, without rollback, specify the pact ID, and provide a yielded value:

```pact
(continue-pact 2 false "[pact-id-hash]" { "rate": 0.9 })
```
