
Use `require-capability` to require a specific `CAPABILITY` to be granted before allowing the current body of code to be executed.
If the required capability isn't found in the environment, the code fails to execute.

By convention, capabilities are defined using all uppercase letters.

## Basic syntax

To test whether a specific `CAPABILITY` has been granted before executing a portion of code in a contract, use the following syntax:

```pact
(require-capability CAPABILITY)
```

## Arguments

Use the following argument to specify the `CAPABILITY` to be tested for using the `require-capability` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `CAPABILITY` |  | Specifies the capability that must be granted before executing a certain portion of code. |

## Return value

The `require-capability` function returns a boolean value indicating whether the specified `CAPABILITY` exists in the environment.

## Examples

The following example demonstrates how to use the `require-capability` function to check whether the capability to transfer funds from one source to another has been granted:

```pact
(require-capability (TRANSFER src dest))
```

If the capability isn't found, the function fails.

The following example uses the `require-capability` function to create a guard that ensure both the GAS and ALLOW_GAS capabilities have been granted:

```pact
(defun gas-payer-guard ()
    (require-capability (GAS))
    (require-capability (ALLOW_GAS))
)
```