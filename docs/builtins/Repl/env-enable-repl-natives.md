## env-enable-repl-natives

Use `env-enable-repl-natives` to control whether REPL native functions are allowed in module code.

### Basic syntax

`(env-enable-repl-natives enable)`

### Arguments

Use the following argument to specify whether to enable or disable REPL native functions in module code.

| Argument | Type | Description |
| --- | --- | --- |
| `enable` | `bool` | Specifies whether to enable or disable REPL native functions in module code. Set to `true` to enable, or `false` to disable. |

### Return value

The `env-enable-repl-natives` function returns a string indicating the status of REPL natives.

- If `enable` is set to `true`, it returns `"Repl natives enabled"`.
- If `enable` is set to `false`, it returns `"Repl natives disabled"`.

### Behavior

When REPL native functions are enabled (`enable` is set to `true`), fixture functions like `env-sigs` are allowed in module code. This means that you can use these functions within your module definitions.

On the other hand, when REPL native functions are disabled (`enable` is set to `false`), fixture functions are not allowed in module code, and attempting to use them will result in an error.

### Example

The following example demonstrates enabling REPL native functions within a Pact REPL:

```pact
pact> (env-enable-repl-natives true)
"Repl natives enabled"
```

After enabling REPL natives, you can use fixture functions like `env-sigs` within your module code.

It's important to note that enabling REPL native functions in module code should be done with caution, as it allows access to functions that are typically intended for use in the REPL environment only. Enable this feature only when necessary and ensure that the usage of REPL native functions in module code is properly controlled and validated.
