## rollback-tx

Use `rollback-tx` to roll back the current transaction.
In most cases, you include this function in `.repl` files that test specific application features and failure scenarios.

Within the context of a transaction started using the `begin-tx` function, you can set environment data, load modules, and execute functions.
You use the `rollback-tx` function to signal the end of a transaction block that needs to be rolled back and can be followed by additional `begin-tx` and `commit-tx` blocks.

### Basic syntax

To roll back a transaction, use the following syntax:

```pact
(rollback-tx)
```

### Arguments

The `rollback-tx` function does not take any arguments.

### Return value

The `rollback-tx` function returns a string indicating the transaction identifier that has been rolled back.

### Examples

The following example demonstrates how to use the `rollback-tx` function within a Pact REPL:

```pact
pact> (begin-tx "load module")
"Begin Tx 0 load module"
pact> (rollback-tx)
"Rollback Tx 0 load module"
```

In this example, a new transaction—Tx0—is started using `(begin-tx)`, and then `(rollback-tx)` is called to roll back the changes from the transaction.
The function returns a string indicating that "Tx 0" has been rolled back.

Note that you should only call the `rollback-tx` function after performing the necessary operations within a transaction.
The `rollback-tx` function finalizes the transaction, and any changes made during the transaction are rolled back.

It's important that you always call the `rollback-tx` function after `begin-tx` and after any other transaction-related operations to properly roll back the transaction.
If `rollback-tx` is not called, the transaction remains open and may lead to unexpected behavior or inconsistencies in the system if you are expecting changes to be undone.
