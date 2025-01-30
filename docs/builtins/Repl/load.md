## load

Use `load` to load and evaluate a specified `.pact` or `.repl` file.
You can reset the REPL state before loading the file by including the optional reset argument set to `true`.
true.

### Basic syntax

To load a specific .pact or .repl filename, use the following syntax:

```pact
(load filename reset)
```

### Arguments

Use the following argument when using the `load` Pact function.

| Argument | Type | Description |
|----------|------|---------|
| filename | string | Specifies the .pact or .repl file you want to load into the Pact REPL.|
| reset    | bool | Resets the REPL state before loading if set to true (optional).|

### Return value

The `load` function returns the unit value `()`.

### Example

The following example demonstrates how to use the `load` function to load the `hello-pact.repl` file to interact with its function in the Pact REPL without resetting the current Pact REPL state.
In this example, the `hello-pact.repl` file consists of one expression `(+ "Hello, " "Pact!")` that is evaluating and returned in the REPL:

```pact
(load "hello-pact.repl")
"Loading hello-pact.repl..."
"Hello, Pact!"
```

If you want to clear the REPL state before loading the file, set the optional `reset` argument to `true`:

```pact
(load "vote-module.pact" true)
```
