## define-keyset
Use `define-keyset` to define a keyset as *`NAME`* with *`KEYSET`*, or if unspecified, read *`NAME`* from the message payload as a keyset, similarly to 'read-keyset'. If the keyset *`NAME`* already exists, the keyset will be enforced before updating to the new value.

### Basic syntax

To define a keyset as *`NAME`* with *`KEYSET`*, or read *`NAME`* from the message payload, use the following syntax:

define-keyset *`NAME`* *`KEYSET`*
define-keyset *`NAME`*

### Arguments

Use the following arguments to specify the inputs for the `define-keyset` Pact function:

| Argument | Type   | Description                                                 |
|----------|--------|-------------------------------------------------------------|
| name     | string | Specifies the name of the keyset to define or read.         |
| keyset   | string | Specifies the keyset to associate with the *`NAME`*.        |

### Return values

The `define-keyset` function returns a string representing the result of defining the keyset.

### Examples

The following examples demonstrate the `define-keyset` function:

1. Define a keyset named 'admin-keyset' with a specified keyset:

```lisp
(define-keyset 'admin-keyset "my-keyset")
```

In this example, `(define-keyset 'admin-keyset "my-keyset")` is used to define a keyset named 'admin-keyset' with the specified keyset "my-keyset".

2. Read the keyset from the message payload and associate it with 'admin-keyset':

```lisp
(define-keyset 'admin-keyset)
```

In this example, `(define-keyset 'admin-keyset)` is used to read the keyset from the message payload and associate it with 'admin-keyset'. This is similar to using `read-keyset` but associates the result with the specified name.
