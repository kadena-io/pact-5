## with-capability

Use `with-capability` to apply the access to a specific capability to execute a body of code.
This function ensures that an elevated privilege—defined as a capability using a `defcap` code block—is present during the execution of the provided body of code. 

You can only call the `with-capability` function in the same module that contains the corresponding `defcap` declaration. 
If the token that grants permission to use the specified capability isn't found, the `with-capability`  evaluates the capability definition to install or grant the permission token. 
The permission token is automatically revoked after executing the code body. 
Nested `with-capability` calls for the same permission token detect the presence of the token and execute the body without reapplying the capability.

By convention, capabilities are defined using all uppercase letters.

### Basic syntax

To request the grant of an acquired `CAPABILITY`, use the following syntax:

```pact
(with-capability CAPABILITY body)
```

### Arguments

Use the following arguments to specify the name of the capability and the body of expressions to be executed using the `with-capability` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `CAPABILITY` | capability | Specifies the name of the capability to grant access to. |
| `body` | any | Specifies the body of expressions to be executed using the granted capability. |

### Return value

The `with-capability` function returns the result of executing the provided body of code using the granted capability.

### Examples

The following example demonstrates how to use the `with-capability` function to request access to the `UPDATE-USERS` capability to execute the code that updates user information:

```pact
(with-capability (UPDATE-USERS id) (update users id { salary: new-salary }))
```

In this example, the `with-capability` function ensures that a sensitive operation can only be executed with an elevated permission granted using the `UPDATE-USERS `capability.
