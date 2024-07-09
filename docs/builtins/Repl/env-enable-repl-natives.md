## env-enable-repl-natives

Use `env-enable-repl-natives` to control whether REPL native functions are allowed in module code.

If you set this function to `true` to enable REPL native functions in module code, other environment configuration functions like `env-sigs` are allowed in module code, and you can use these functions in your module definitions.

If you set this function to `false` to disable REPL native functions in module code, other environment configuration functions are not allowed in module code, and attempting to use them will result in an error.

### Basic syntax

To allow REPL native functions to be used in module code, use the following syntax:

```pact
(env-enable-repl-natives enable)
```

### Arguments

Use the following argument to specify whether to enable or disable REPL native functions in module code.

| Argument | Type | Description |
| --- | --- | --- |
| `enable` | bool | Specifies whether to enable or disable REPL native functions in module code. Note that this argument is required. Set it to `true` to enable REPL native function calls. Set it to `false` to prevent REPL native functions from being called. |

### Return value

The `env-enable-repl-natives` function returns a string indicating the status of REPL natives.

### Examples

The following example demonstrates how to enable REPL native functions:

```pact
(env-enable-repl-natives true)
"Repl natives enabled"
```

After enabling REPL natives, you can use environment configuration functions like `env-sigs` in your module code.

It's important to note that you should only enable REPL native functions in module code—that is, executable `.pact` files—if absolutely necessary. 
Most REPL native functions are intended to be used exclusively in the REPL environment. 
If you enable access to the REPL native functions in module code, ensure that access to the functions is properly controlled and validated.

The following example demonstrates how to disable access to REPL native functions:

```pact
(env-enable-repl-natives false)
"Repl natives disabled"
```
