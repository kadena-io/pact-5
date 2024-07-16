## begin-tx

Use `begin-tx` to begin a new transaction with an optional name.
This function is used to create tests that you want to execute using the Pact REPL.
In most cases, you include this function in `.repl` files that test specific application features and failure scenarios.

Within the context of a transaction, you can set environment data, load modules, and execute functions.
To complete the transaction, use the `commit-tx` function. The `commit-tx` function signals the end of a transaction block and can be followed by additional `begin-tx` and `commit-tx` blocks.

### Basic syntax

To begin a transaction without a name, use the following syntax:

```pact
(begin-tx)
```

To begin a transaction with a specific name, use the following syntax:

```pact
(begin-tx name)
```

## Arguments

Use the following argument to specify an optional name for the transaction when using the `begin-tx` Pact function.

| Argument | Type   | Description                                      |
|----------|--------|--------------------------------------------------|
| `name` | string | Specifies the name of the transaction (optional). |

## Return value

The `begin-tx` function returns a string indicating the transaction identifier and the optional name (if provided).

## Examples

The following example demonstrates how to use the `begin-tx` function to begin a new transaction without a name:

```pact
pact> (begin-tx)
"Begin Tx 0"
```

The following example demonstrates how to use the `begin-tx` function to begin a new transaction with a specific name:

```pact
pact> (begin-tx "load module")
"Begin Tx 0: load module"
```

You can write an empty transaction as a placeholder for the logic to test specific function or failure scenarios.
For example, to create a placeholder for the logic that defines a namespace:

```pact
(begin-tx "Define a namespace called 'election")

(commit-tx)
```