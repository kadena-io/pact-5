The `with-capability` function specifies and requests the grant of an acquired capability, which is an application of a 'defcap' production. It ensures that the specified unique token granted by this application is present in the environment during the execution of the provided body. 

`with-capability` can only be called in the same module that declares the corresponding 'defcap'. If the token is not present, the capability is evaluated, and upon successful completion, the token is installed or granted in the environment. The token will be automatically revoked upon completion of the body. Nested `with-capability` calls for the same token detect the presence of the token and execute the body without reapplying the capability.

### Basic syntax

To specify and request the grant of an acquired capability, use the following syntax:

with-capability *capability* *body*

### Arguments

Use the following arguments to specify the capability and the body for execution using the `with-capability` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| capability | capability | Specifies the capability to be acquired. |
| body | [*] | Specifies the body of expressions to be executed under the granted capability. |

### Return value

The `with-capability` function returns the result of executing the provided body under the granted capability.

### Examples

The following example demonstrates the usage of the `with-capability` function within a Pact script. It requests the grant of an 'UPDATE-USERS' capability and executes the body, updating user information:

```lisp
(with-capability (UPDATE-USERS id) (update users id { salary: new-salary }))
```

This example illustrates how to use the `with-capability` function to ensure the execution of specific operations under the granted capability in Pact, providing a controlled access mechanism to sensitive functionalities.
