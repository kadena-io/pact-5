## sort
The `sort` function is used to sort a list of primitive values or objects based on either the values themselves or specific fields within the objects.

### Basic syntax

To sort a list of primitive values, use the following syntax:

sort *values*

To sort a list of objects based on specific fields, use the following syntax:

sort *fields* *values*

### Arguments

Use the following arguments to specify the values or fields for sorting using the `sort` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| values | [\<a>] | Specifies the list of primitive values to be sorted. |
| fields | [string] | Specifies the list of fields within objects to be used for sorting. |

### Return value

The `sort` function returns a sorted list of values or objects based on the specified sorting criteria.

### Examples

The following examples demonstrate the usage of the `sort` function within a Pact script.

To sort a list of primitive values `[3, 1, 2]`:

```lisp
(sort [3 1 2])
```

To sort a list of objects `[{ 'name: "Lin", 'age: 30 }, { 'name: "Val", 'age: 25 }]` based on the `'age` field:

```lisp
(sort ['age] [{'name: "Lin", 'age: 30} {'name: "Val", 'age: 25}])
```

These examples illustrate how to use the `sort` function to sort lists of values or objects in Pact, either directly or based on specific fields within the objects.
