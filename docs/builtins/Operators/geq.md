## >=

The `>=` function returns true if the first argument `x` is greater than or equal to the second argument `y`.

### Basic syntax

To check if `x` is greater than or equal to `y`, use the following syntax:

`(>= x y)`

### Arguments

Use the following arguments to specify the values for comparison using the `>=` Pact function.

| Argument | Type                               | Description                                |
| -------- | ---------------------------------- | ------------------------------------------ |
| `x`      | `<a[integer,decimal,string,time]>` | Specifies the first value for comparison.  |
| `y`      | `<a[integer,decimal,string,time]>` | Specifies the second value for comparison. |

### Return value

The `>=` function returns a boolean value indicating whether `x` is greater than or equal to `y`.

### Examples

The following examples demonstrate the usage of the `>=` function within a Pact REPL. They compare two values to check if the first value is greater than or equal to the second value:

```pact
pact>(>= 1 3)
false
```

```pact
pact>(>= 5.24 2.52)
true
```

```pact
pact>(>= "abc" "def")
false
```

These examples illustrate how to use the `>=` function to perform comparison operations in Pact, facilitating logical comparisons between different types of values.
