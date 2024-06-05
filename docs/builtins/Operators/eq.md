## =

The `=` function returns true if the first argument `x` is equal to the second argument `y`.

### Basic syntax

To check if `x` is equal to `y`, use the following syntax:

`(= x y)`

### Arguments

Use the following arguments to specify the values for comparison using the `=` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `x` | `<a[integer,decimal,string,time,bool,object,list,table]>` | Specifies the first value for comparison. |
| `y` | `<a[integer,decimal,string,time,bool,object,list,table]>` | Specifies the second value for comparison. |

### Return value

The `=` function returns a boolean value indicating whether `x` is equal to `y`.

### Examples

The following examples demonstrate the usage of the `=` function within a Pact REPL. They compare two values to check if the first value is equal to the second value:

```pact
pact>(= 5 5)
true
```

```pact
pact>(= 3.14 2.71)
false
```

```pact
pact>(= "hello" "hello")
true
```

```pact
pact>(= (time "2023-06-05T10:00:00Z") (time "2023-06-05T10:00:00Z"))
true
```

```pact
pact>(= true false)
false
```

```pact
pact>(= { "name": "Alice", "age": 30 } { "name": "Alice", "age": 30 })
true
```

```pact
pact>(= [1, 2, 3] [1, 2, 3])
true
```
