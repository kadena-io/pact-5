## install-capabiliy
Use `install-capability` to specify and provision the installation of a managed CAPABILITY. Managed capabilities are defined within a 'defcap' block, where a '@managed' tag designates a single parameter to be managed by a specified function. After installation, the CAPABILITY must still be brought into scope using 'with-capability', at which time the 'manager function' is invoked to validate the request.

The manager function is of type `managed:<p> requested:<p> -> <p>`, where `<p>` indicates the type of the managed parameter. For example, if you have `(defcap FOO (bar:string baz:integer) @managed baz FOO-mgr ...)`, the manager function would be `(defun FOO-mgr:integer (managed:integer requested:integer) ...)`. 

Any capability matching the `static` (non-managed) parameters will cause this function to be invoked with the current managed value and that of the requested capability. The function should perform whatever logic, presumably linear, to validate the request, and return the new managed value representing the 'balance' of the request.

Note that signatures scoped to a managed capability cause the capability to be automatically provisioned for installation similarly to one installed with this function.

### Basic syntax

To specify and provision the installation of a managed capability, use the following syntax:

`(install-capability capability)`

### Arguments

Use the following argument to specify the capability you want to install using the `install-capability` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `capability` | `any` | Specifies the capability to be installed. |

### Return value

The `install-capability` function returns a boolean value indicating the success or failure of the installation, along with a string message providing additional information.

### Examples

The following example demonstrates the use of `install-capability` in the Pact REPL to install a capability named `PAY` with specified parameters:

```pact
pact>(install-capability (PAY "alice" "bob" 10.0))
```

In this example, the capability named `PAY` with the provided parameters is installed. If successful, it returns a boolean value indicating success; otherwise, it returns an error message indicating the reason for failure.
