## take
The `take` function retrieves a specified number of values from a list (or string), or entries with keys specified in *`KEYS`* from an object. If *`COUNT`* is negative, values are taken from the end of the list. If *`COUNT`* exceeds the interval (-2^63,2^63), it is truncated to that range.

### Basic syntax

To retrieve a specified number of values from a list or string, use the following syntax:

take *count* *list*

To retrieve entries with specified keys from an object, use the following syntax:

take *keys* *object*

### Arguments

Use the following arguments to specify the count, list (or string), keys, or object for retrieval using the `take` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| count | integer | Specifies the number of values to retrieve from the list (or string). If negative, values are taken from the end. |
| list | [\<a[[\<l>],string]>] | Specifies the list or string from which to retrieve values. |
| keys | [string] | Specifies the keys of entries to retrieve from the object. |
| object | object:<{o}> | Specifies the object from which to retrieve entries. |

### Return value

The `take` function returns a sublist of values from the list (or string) or a subset of entries from the object based on the specified count or keys.

### Examples

The following examples demonstrate the usage of the `take` function within a Pact script.

To retrieve the first 2 characters from the string "abcd":

```lisp
(take 2 "abcd")
```

To retrieve the last 3 values from the list [1, 2, 3, 4, 5]:

```lisp
(take (- 3) [1 2 3 4 5])
```

To retrieve entries with the key `'name` from the object `{ 'name: "Vlad", 'active: false }`:

```lisp
(take ['name] { 'name: "Vlad", 'active: false })
```

These examples illustrate how to use the `take` function to retrieve specified values or entries from lists, strings, or objects in Pact.