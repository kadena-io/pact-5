## equal (=)

Use `=` to return true if the first argument `oper1` is equal to the second argument `oper2`.

### Basic syntax

To check if `oper1` is equal to `oper2`, use the following syntax:

`(= oper1 oper2)`

### Arguments

Use the following arguments to specify the values for comparison using the `=` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `oper1` | integer, decimal, string, time, bool, object, list, modref, guard | Specifies the first value for comparison. |
| `oper2` | integer, decimal, string, time, bool, object, list, modref, guard | Specifies the second value for comparison. |

### Return value

The `=` function returns a boolean value indicating whether `oper1` is equal to `oper2`.

### Examples

The following example demonstrates how to use the `=` function to compare two integer values to check if the first value is equal to the second value:

```pact
pact> (= 5 5)
true
```

The following example demonstrates how to use the `=` function to compare two decimal values to check if the first value is equal to the second value:

```pact
pact> (= 3.14 2.71)
false
```

The following example demonstrates how to use the `=` function to compare two string values to check if the first string is equal to the second string:

```pact
pact> (= "hello" "hello")
true
```

The following example demonstrates how to use the `=` function to compare two time values to check if the first time is equal to the second time:

```pact
pact> (= (time "2023-06-05T10:00:00Z") (time "2023-06-05T10:00:00Z"))
true
```

The following example demonstrates how to use the `=` function to compare two object values to check if the first object is equal to the second object:

```pact
pact> (= { "name": "Alice", "age": 30 } { "name": "Alice", "age": 26 })
false
```

The following example demonstrates how to use the `=` function to compare two list values to check if the first list is equal to the second list:

```pact
pact> (= [1, 2, 3] [1, 2, 3])
true
```

You can also the `=` function to evaluate variables and expressions.
For example:

```pact
(enforce (= amount 1.0) "Mint can only be 1")
```
