## env-events

Use `env-events` to retrieve any accumulated events and optionally clear the event state.

### Basic syntax

To retrieve events and clear the event state, use the following syntax:

```pact
(env-events clear)
```

## Arguments

Use the following argument to specify whether to clear the event state after retrieving the events.

| Argument | Type | Description |
| --- | --- | --- |
| `clear` | bool | Specifies whether to clear the event state after retrieving the events. Note that this argument is required. Set it to `true` to retrieve events, then clear the event state. Set it to `false` retrieve events and keep the event state. |

## Return value

The `env-events` function returns an array of objects representing the accumulated events. 
Each object in the array has the following fields:

- `name`: The fully-qualified name of the event.
- `params`: The parameters associated with the event.
- `module-hash`: The hash of the module that emitted the event.

### Examples

The following example demonstrates how to retrieve events, then clear the event state:

```pact
(env-events true)
[]
```

In this example, there were no events to retrieve, so an empty list is returned.

The following example retrieves three events—TOKEN, MINT, and ACCOUNT-GUARD—without clearing the event state:

```pact
(env-events false)
[{"module-hash": "DmPuO814Zw0C6RL9ubXTGeyNl0I2-svVHgZp-XqRsOs",
"name": "marmalade-v2.ledger.TOKEN",
"params": ["t:U50F3xof5EnLQFPd0v2vt8PR3GJTAt8DJ2oWFj7eOgA" 0 [marmalade-v2.non-fungible-policy-v1 marmalade-v2.non-updatable-uri-policy-v1] "uri" KeySet {keys: [e4c6807d79d8bf4695e10e5678ebf72862f59b71f971d39dd3349f4beeacd6e3],pred: keys-all}]} 
{"module-hash": "DmPuO814Zw0C6RL9ubXTGeyNl0I2-svVHgZp-XqRsOs",
"name": "marmalade-v2.ledger.MINT",
"params": ["t:U50F3xof5EnLQFPd0v2vt8PR3GJTAt8DJ2oWFj7eOgA" "k:e4c6807d79d8bf4695e10e5678ebf72862f59b71f971d39dd3349f4beeacd6e3" 1.0]} 
{"module-hash": "DmPuO814Zw0C6RL9ubXTGeyNl0I2-svVHgZp-XqRsOs",
"name": "marmalade-v2.ledger.ACCOUNT_GUARD",
"params": ["t:U50F3xof5EnLQFPd0v2vt8PR3GJTAt8DJ2oWFj7eOgA" "k:e4c6807d79d8bf4695e10e5678ebf72862f59b71f971d39dd3349f4beeacd6e3" KeySet {keys: [e4c6807d79d8bf4695e10e5678ebf72862f59b71f971d39dd3349f4beeacd6e3],pred: keys-all}]}]

In this example, the formatting is modified for readability.