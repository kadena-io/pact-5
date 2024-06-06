## sort

Use `sort` to sort a list of primitive values based on the values themselves or a list of objects based on specific fields within the objects.
If you're sorting primitive values, they are sorted in ascending numerical or alphabetical order.

### Basic syntax

Because `sort` is an overloaded function, there are two ways to use it.
To sort a list of primitive values, use the following syntax:

```pact
(sort [primatives])
```

To sort a list of objects based on specific fields, use the following syntax:

```pact
(sort [fields] [{object1} {object2} ...])
```

### Arguments

Use the following arguments to specify the values or fields for sorting using the `sort` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `primitives` | [integer, decimal, or string] | Specifies the list of primitive values to be sorted. |
| `fields` | [list] | Specifies the list of fields within objects to be used for sorting. |
| `object1` | {object} | Specifies more or more objects to be sorted using the `fields` parameter.

### Return value

The `sort` function returns a sorted list of values or objects based on the specified sorting criteria.

### Examples

The following examples illustrate how to use the `sort` function to sort lists of values or objects in Pact.

The following example demonstrates how to the sort a list of primitive values `[3, 1, 2]` using of the `sort` function:

```pact
pact> (sort [3 1 2])
[1, 2, 3]
```

If you sort a list of string primitive values, they are sorted in alphabetical order.
For example:

```pact
pact> (sort ["second" "day" "minute" "hour"])
["day" "hour" "minute" "second"]
```

To sort a list of objects based on the `'age` field:

```pact
pact> (sort ['age] [{'name: "Lin", 'age: 30} {'name: "Val", 'age: 25} {'name: "Kai", 'age: 21}])
[{"name": "Kai","age": 21} {"name": "Val","age": 25} {"name": "Lin","age": 30}]
```
