## where

Use the `where` function to apply a specified application function (`app`) to a field (`field`) to evaluate the field value (`value`).
The function returns a boolean value based on the result of the application.
This function is most often used in conjunction with `filter` and `select` operations.

### Basic syntax

To apply an application function to a `field` in a value, use the following syntax:

```pact
(where field app)
```

### Arguments

Use the following arguments to specify the field, application function, and value for evaluation using the `where` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `field` | string | Specifies the field in the value to be evaluated. |
| `app` | function | Specifies the application function to be applied to the field. |
| `value` | `object:<{row}>` | Specifies the value containing the field to be evaluated. |

### Return value

The `where` function returns a `boolean` value based on the result of applying the specified application function to the field in the value.

### Examples

The following example demonstrates how to use the `where` function in the Pact REPL. 
This example applies a condition for filtering a list of objects where the `"age"` field value is less than `20`:

```pact
(filter (where 'age (> 20)) [{'name: "Mary",'age: 30} {'name: "Juan",'age: 15}])
[{"age":15, "name":"Juan"}]
```
