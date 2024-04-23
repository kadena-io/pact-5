Use `drop` to remove *`COUNT`* values from a *`LIST`* (or string), or entries having keys in *`KEYS`* from an *`OBJECT`*.

## Basic syntax

To remove *`COUNT`* values from a *`LIST`* (or string), use the following syntax:

drop *count* *list* -> *list*
drop *keys* *object* -> *object*

## Arguments

Use the following arguments to specify the *`COUNT`*, *`LIST`*, or *`KEYS`* for the `drop` Pact function:

| Argument | Type           | Description                                                          |
|----------|----------------|----------------------------------------------------------------------|
| count    | integer        | Specifies the number of values to drop from the list (or string).     |
| list     | [<a>], string | Specifies the list (or string) from which to drop values.             |
| keys     | [string]       | Specifies the keys to drop from the object.                          |
| object   | object:<{o}>   | Specifies the object from which to drop entries with specified keys.  |

## Return values

The `drop` function returns the modified list (or string) after dropping the specified number of values, or the object after dropping entries with specified keys.

## Examples

The following examples demonstrate the `drop` function:

1. Drop 2 values from the string "vwxyz":

```lisp
(drop 2 "vwxyz")
```

2. Drop 2 values from the end of the list [1 2 3 4 5]:

```lisp
(drop (- 2) [1 2 3 4 5])
```

3. Drop entries with the key 'name' from the object { 'name': "Vlad", 'active': false }:

```lisp
(drop ['name] { 'name': "Vlad", 'active': false })
```

In these examples, the `drop` function is used to remove values or entries from the specified list, string, or object based on the given count or keys. The resulting list, string, or object will have the specified values or entries removed.
