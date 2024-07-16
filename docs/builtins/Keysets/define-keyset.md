## define-keyset

Use `define-keyset` to define a keyset with the specified `name` and `keyset` guard.

If you don't specify a keyset guard, the functions reads the specified keyset `name` from the message payload to define its keyset guard, which  is similar to using the `read-keyset` function. 
If the keyset `name` already exists, the existing keyset guard is enforced before updating to the new value.

### Basic syntax

To define a keyset as `name` with `keyset`, use the following syntax:

```pact
(define-keyset name keyset)
```

To read keyset information from the transaction message payload, use the following syntax:

```pact
(define-keyset name)
```

### Arguments

Use the following arguments to specify the inputs for the `define-keyset` Pact function:

| Argument | Type | Description |
|----------|------|-------------|
| `name` | string | Specifies the name of the keyset to define or read. |
| `keyset` | string | Specifies the keyset to associate with the `name`. |

### Return values

The `define-keyset` function returns a string representing the result of defining the keyset.

### Examples

The following example demonstrates how to use the `define-keyset` function to define a keyset named "admin-keyset" to use the keys and predicate function from the specified "my-keyset" object:

```pact
(define-keyset 'admin-keyset "my-keyset")
```

The following example demonstrates how to read the keyset from the message payload and associate it with `admin-keyset`:

```pact
(define-keyset 'admin-keyset)
```

The following example illustrates how to define a keyset by reading an existing keyset:

```pact
(define-keyset "admin-keyset" (read-keyset 'admin-keyset))
```