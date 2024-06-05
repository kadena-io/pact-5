## begin-tx

Use `begin-tx` to begin a new transaction with an optional name.

### Basic syntax

To begin a transaction without a name, use the following syntax:

```lisp
(begin-tx)
```

To begin a transaction with a specific name, use the following syntax:

```lisp
(begin-tx name)
```

## Arguments

Use the following argument to specify an optional name for the transaction when using the `begin-tx` Pact function.

| Argument | Type   | Description                                      |
|----------|--------|--------------------------------------------------|
| `name`     | `string` | (Optional) Specifies the name of the transaction. |

## Return value

The `begin-tx` function returns a string indicating the transaction identifier and the optional name (if provided).

## Examples

The following examples demonstrate the usage of the `begin-tx` function within a Pact REPL:

1. Beginning a transaction without a name:

```pact
pact> (begin-tx)
"Begin Tx 0"
```


2. Beginning a transaction with a name:

```pact
pact> (begin-tx "load module")
"Begin Tx 0: load module"
```

By using `begin-tx`, you can initiate a new transaction in Pact and optionally assign a name to it for better organization and tracking purposes.
