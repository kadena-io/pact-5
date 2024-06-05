## define-namespace

Use `define-namespace` to create a new namespace or update the guards of an existing namespace. The ownership and use of the namespace are controlled by the specified guards.

**Note:** This function can only be used at the top level of your code. It will fail if used within module code.

### Basic syntax

`(define-namespace namespace user-guard admin-guard)`

### Arguments

Use the following arguments to specify the namespace and guards when using the `define-namespace` Pact function.

| Argument    | Type   | Description                                                                         |
|-------------|--------|------------------------------------------------------------------------------------|
| `namespace`   | `string` | Specifies the name of the namespace to create or update.                           |
| `user-guard`  | `guard`  | Specifies the guard controlling the use of the namespace.                          |
| `admin-guard` | `guard`  | Specifies the guard controlling the ownership and administrative tasks in the namespace. |

### Return value

The `define-namespace` function returns the name of the created or updated namespace as a string.

### Example

The following example creates a new namespace called `'my-namespace'` with user and admin guards defined by the `'user-ks'` and `'admin-ks'` keysets, respectively:

```pact
(define-namespace 'my-namespace (read-keyset 'user-ks) (read-keyset 'admin-ks))
```
