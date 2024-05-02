The `require-capability` function specifies and tests for the existing grant of a specified *`CAPABILITY`*, failing if it is not found in the environment.

## Basic syntax

To specify and test for the existing grant of a *`CAPABILITY`*, use the following syntax:

require-capability *capability*

## Arguments

Use the following argument to specify the *`CAPABILITY`* to be tested for its existing grant using the `require-capability` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| capability |  | Specifies the capability to be tested for its existing grant. |

## Return value

The `require-capability` function returns a boolean value indicating whether the specified *`CAPABILITY`* exists in the environment.

## Example

The following example demonstrates the usage of the `require-capability` function within a Pact script. It tests for the existing grant of the capability to transfer funds from one source to another:

```lisp
(require-capability (TRANSFER src dest))
```

This example illustrates how to use the `require-capability` function to check for the existing grant of a specific capability in the environment. If the capability is not found, the function will fail.
