## typeof

Use `typeof` to return the data type of the specified `value`.
The data type of the specified value is `returned` as a string.

### Basic syntax

To determine the data type for a `value`, use the following syntax:

`(typeof value)`

### Argument

Use the following argument to specify the value for which to determine the type using the `typeof` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `value` | any | Specifies the value for which to determine the type. |

### Return value

The `typeof` function returns the data type of the provided `value` as a string.

### Examples

The following example demonstrates how to use the `typeof` function in the Pact REPL. 
This example returns the data type of the value `'hello`:

```pact
pact> (typeof 'hello)
"string"
```

The following example returns the data type of the value `8`:

```pact
pact> (typeof 8)
"integer"
```

The following example returns the data type of the value `4.3`:

```pact
pact> (typeof 4.3)
"decimal"
```

