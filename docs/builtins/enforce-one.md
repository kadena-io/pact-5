Use `enforce-one` to run a series of tests in order (in a pure context, plus keyset enforces). If all tests fail, the transaction fails. This function short-circuits on the first successful test.

### Basic syntax

To run a series of tests in order and short-circuit on the first successful test, use the following syntax:

enforce-one *msg* *tests* -> bool

### Arguments

Use the following arguments to specify the error message and tests for the `enforce-one` Pact function:

| Argument | Type         | Description                                                   |
|----------|--------------|---------------------------------------------------------------|
| msg      | string       | Specifies the error message if all tests fail.                |
| tests    | [bool]       | Specifies the list of tests to run in order.                  |

### Return values

The `enforce-one` function returns true if at least one test succeeds. If all tests fail, it fails the transaction with the specified error message.

### Examples

The following example demonstrates the `enforce-one` function:

```lisp
(enforce-one "Should succeed on second test" [(enforce false "Skip me") (enforce (= (+ 2 2) 4) "Chaos reigns")])
```

In this example, `(enforce-one "Should succeed on second test" [(enforce false "Skip me") (enforce (= (+ 2 2) 4) "Chaos reigns")])` is used to run two tests in order. The first test is `enforce false "Skip me"`, which fails intentionally. The second test is `(enforce (= (+ 2 2) 4) "Chaos reigns")`, which succeeds (`4 = 4` is true). Since the second test succeeds, the `enforce-one` function returns true. If all tests had failed, the transaction would have failed with the specified error message "Should succeed on second test". The `enforce-one` function provides a way to check multiple conditions and short-circuit on the first success.
