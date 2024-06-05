## take

Use the `take` function to retrieve a specified number of values from a list (or string), or entries with keys specified in `keys` from an object. 
If the `count` argument is negative, values are taken from the end of the list. 
If the `count` exceeds the interval (-2^63,2^63), it is truncated to that range.

### Basic syntax

Because `take` is an overloaded function, there are two ways to use it.
To retrieve a specified number of values from a list or a string, use the following syntax:

```pact
(take count [list])
```

To retrieve entries with specified keys from an object, use the following syntax:

```pact
(take keys {object})
```

### Arguments

Use the following arguments to specify the conumber of values to retrieve and the list or object to retrieve them from using the `take` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `count` | integer | Specifies the number of values to retrieve from the list or string. If negative, values are taken from the end. |
| `list` | [list] or string | Specifies the list or string from which to retrieve values. |
| `keys` | [string] | Specifies the keys for the information you want to retrieve from the specified object. |
| `object` | object | Specifies the object from which to retrieve entries. |

### Return value

The `take` function returns a subset of values retrieved from the list or from the object based on the specified count or number of keys.

### Examples

The following examples demonstrate how to use the `take` function in the Pact REPL.

To retrieve the first two characters from the string "abcd":

```pact
pact> (take 2 "abcd")
"ab"
```

To retrieve the last three values from the list [1, 2, 3, 4, 5]:

```pact
pact> (take (- 3) [1 2 3 4 5])
[3, 4, 5]
```

To retrieve entries with the key `'name` from the object `{ 'name: "Vlad", 'active: false }`:

```pact
(take ['name] { 'name: "Vlad", 'active: false, 'age: 34 })
{"name": "Vlad"}
```
