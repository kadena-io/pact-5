## at

Use `at` to retrieve the value at the location specified by an `index` number or by a `key` string in a collection.
If you specify an `index` number, the collection must be a list of values.
If you specify a `key` string, the collection must be an object.

### Basic syntax

Because `at` is an overloaded function, there are two ways to use it.
To get a value using the specified `index` location from a list of values, use the following syntax:

```pact
(at index [list])
```

To get a value using the specified `key` string from an object, use the following syntax:

```pact
(at key {object})
```

### Arguments

Use one of the following argument to define the value you want to retrieve using the `at` Pact function.

| Argument | Type | Description
| -------- | ---- | -----------
| `index` | integer | Specifies the information you want to retrieve. If you specify an `index` number, the function returns the value from that location in a list of values.
| `list` | [integer or decimal] | Specifies the list of values to retrieve the value from using the specified `index` location. 
| `key` | string | Specifies the information you want to retrieve. If you specify a `key` string, the function returns the value corresponding to that key from an object.
| `object` | object | Specifies the set of key and value pairs to retrieve the value from using the specified `key`.

### Return value

The `at` function returns the value found at the specified `index` or using the specified `key`.

The return value can be any data type.

### Examples

The following example returns the value found at the `index` location—starting with 0—from a list of values.
For example, the value at `index` location `3` is `14`:

```pact
(at  3 [20 18 16 14 12 10])
14
```

You can use the `at` function to return any type of data from a list.
For example:

```pact
(at 1 ["blue","green","red","yellow"])
"green"
```

The following example returns the value found at the specified *key* from an object:

```pact
(at "last-name" { "first-name": "maya", "last-name": "tea"})
"tea"
```

You can use the `at` function to return any type of data using the specified *key* from an object.
For example:

```pact
(at "chainId" { "networkId": "development", "chainId": 1, "auth": 0})
1
```
