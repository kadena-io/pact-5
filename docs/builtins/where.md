## where
The `where` function is a utility primarily used in conjunction with `filter` and `select` operations. It applies a specified application function (`APP`) to a field (`FIELD`) in a given value (`VALUE`), returning a boolean value based on the result of the application.

### Basic syntax

To apply an application function to a `FIELD` in a value, use the following syntax:

`(where FIELD APP)`

### Arguments

Use the following arguments to specify the field, application function, and value for evaluation using the `where` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `FIELD` | `string` | Specifies the field in the value to be evaluated. |
| `APP` | `x:<a>` | Specifies the application function to be applied to the field. |
| `VALUE` | `object:<{row}>` | Specifies the value containing the field to be evaluated. |

### Return value

The `where` function returns a `boolean` value based on the result of applying the specified application function to the field in the value.

### Examples

The following example demonstrates the usage of the `where` function within a Pact script. It applies a condition where the field `"age"` in a value should be greater than `20`:

```lisp
(filter (where 'age (> 20)) [{'name: "Mary",'age: 30} {'name: "Juan",'age: 15}])
[{"age":15, "name":"Juan"}]
```

This example illustrates how to use the `where` function to filter values based on a specified condition in Pact, allowing for selective retrieval or processing of data based on field values.
