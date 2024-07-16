## expect-failure

Use `expect-failure` to evaluate an expression and succeed only if the expressions results in an error.
This function enables you to verify that use cases that should failbehave as expected.

### Basic syntax

To expect a failure without specifying the error message, use the following syntax:

```pact
(expect-failure doc exp)
```

To expect a failure with a specific error message, use the following syntax:

```pact
(expect-failure doc err exp)
```

## Arguments

Use the following arguments when using the `expect-failure` Pact function.

| Argument | Type | Description |
|----------|------|-------------|
| `doc` | string | Specifies the documentation string describing the expected failure. |
| `err` | string | Specifies the expected error message (optional). |
| `exp` | any  | Specifies the expression to evaluate. The expression can be of any Pact type.|

## Return value

The `expect-failure` function returns a string indicating the success or failure of the expected failure.

- If the expression `exp` throws an error, the function returns a string indicating the success of the expected failure, for example, `"Expect failure: success: Enforce fails on false"`.
- If the expression `exp` does not throw an error, the function itself throws an error, indicating that the expected failure did not occur.

## Examples

The following example demonstrates how to use the `expect-failure` function without specifying the expected error message:

```pact
pact> (expect-failure "Enforce fails on false" (enforce false "Expected error"))
"Expect failure: success: Enforce fails on false"
```

The following example illustrates using the `expect-failure` function with a specific expected error message:

```pact
(expect-failure "mint fails without MINT capability in scope"
    "Managed capability not installed: (marmalade-v2.ledger.MINT"
    (mint (read-msg 'token-id) (read-msg 'account) (read-keyset 'account-guard) 1.0)))
```

In this example, the `doc` argument describes the test being performed.
The `err` argument specifies the error message that is returned when the expression is evaluated. 
