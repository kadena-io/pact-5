## define-keyset

Use `define-keyset` to define a keyset as `name` with `keyset`, or if unspecified, read `name` from the message payload as a keyset, similarly to `read-keyset`. If the keyset `name` already exists, the keyset will be enforced before updating to the new value.

### Basic syntax

To define a keyset as `name` with `keyset`, or read `name` from the message payload, use the following syntax:

`(define-keyset name keyset)`
`(define-keyset name)`

### Arguments

Use the following arguments to specify the inputs for the `define-keyset` Pact function:

| Argument | Type   | Description                                                 |
|----------|--------|-------------------------------------------------------------|
| `name`     | `string` | Specifies the name of the keyset to define or read.         |
| `keyset`   | `string` | Specifies the keyset to associate with the `name`.        |

### Return values

The `define-keyset` function returns a string representing the result of defining the keyset.

### Examples

The following examples demonstrate the `define-keyset` function:

1. Define a keyset named 'admin-keyset' with a specified keyset:

```pact
(define-keyset 'admin-keyset "my-keyset")
```

2. Read the keyset from the message payload and associate it with 'admin-keyset':

```pact
(define-keyset 'admin-keyset)
```
