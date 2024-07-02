## expect-failure

Use `expect-failure` to evaluate an expression and succeed only if it throws an error.

### Basic syntax

To expect a failure without specifying the error message, use the following syntax:

```pact
(expect-failure doc exp)
```

To expect a failure with a specific error message, use the following syntax:

```pact
(expect-failure doc err exp)
```

### Arguments

Use the following arguments when using the `expect-failure` Pact function.

| Argument | Type   | Description                                                                    |
|----------|--------|--------------------------------------------------------------------------------|
| `doc`      | `string` | Specifies the documentation string describing the expected failure.        |
| `err`      | `string` | (Optional) Specifies the expected error message.                           |
| `exp`      | `<a>`  | Specifies the expression to evaluate. The expression can be of any Pact type.|

### Return value

The `expect-failure` function returns a string indicating the success or failure of the expected failure.

- If the expression `exp` throws an error, the function returns a string indicating the success of the expected failure, e.g., `"Expect failure: success: Enforce fails on false"`.
- If the expression `exp` does not throw an error, the function itself throws an error, indicating that the expected failure did not occur.

### Examples

The following examples demonstrate the usage of `expect-failure` within a Pact REPL:

1. Expecting a failure without specifying the error message:
```pact
pact> (expect-failure "Enforce fails on false" (enforce false "Expected error"))
"Expect failure: success: Enforce fails on false"
```

2. Expecting a failure with a specific error message:
```pact
pact> (expect-failure "Enforce fails with message" "Expected error" (enforce false "Expected error"))
"Expect failure: success: Enforce fails with message"
```
