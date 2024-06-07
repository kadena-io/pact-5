## enforce-one

Use `enforce-one` to run a series of tests in order (in a pure context, plus keyset enforces). 
If all tests fail, the transaction fails. This function short-circuits on the first successful test.

### Basic syntax

To run a series of tests in order and short-circuit on the first successful test, use the following syntax:

```pact
(enforce-one msg tests)
```

### Arguments

Use the following arguments to specify the error message and tests for the `enforce-one` Pact function:

| Argument | Type | Description |
|----------|------|------------ |
| `msg`  | string | Specifies the error message if all tests fail. |
| `tests` | bool | Specifies the list of tests to run in order. |

### Return values

The `enforce-one` function returns `true` if at least one test succeeds. 
If all tests fail, the function fails the transaction with the specified error message.

### Examples

The following example demonstrates the `enforce-one` function:

```pact
pact> (enforce-one "Should succeed on second test" [(enforce false "Skip me") (enforce (= (+ 2 2) 4) "Chaos reigns")])
true
```

In this example, `(enforce-one "Should succeed on second test" [(enforce false "Skip me") (enforce (= (+ 2 2) 4) "Chaos reigns")])` is used to run two tests in order. 
The first test is `enforce false "Skip me"`, which fails intentionally. 
The second test is `(enforce (= (+ 2 2) 4) "Chaos reigns")`, which succeeds because `4 = 4` is true. 
Because the second test succeeds, the `enforce-one` function returns `true`. 

If all tests had failed, the transaction would have failed with the specified error message "Should succeed on second test". 
