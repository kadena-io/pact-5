## add
The `+` function performs addition for numbers, concatenation for strings/lists, or merging for objects.

### Basic syntax

To perform addition for numbers, use the following syntax:

+ *x* *y*

To concatenate strings/lists or merge objects, use the following syntax:

+ *x* *y*

### Arguments

Use the following arguments to specify the values for addition, concatenation, or merging using the `+` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| x | <a[integer,decimal,string,[<l>],object:<{o}>]> | Specifies the first operand. |
| y | <a[integer,decimal,string,[<l>],object:<{o}>]> | Specifies the second operand. |

### Return value

The `+` function returns the result of addition for numbers, concatenation for strings/lists, or merging for objects.

### Examples

The following examples demonstrate the usage of the `+` function within a Pact script. They perform addition, concatenation, or merging:

```lisp
(+ 1 2)
```
```lisp
(+ 5.0 0.5)
```
```lisp
(+ "every" "body")
```
```lisp
(+ [1 2] [3 4])
```
```lisp
(+ { "foo": 100 } { "foo": 1, "bar": 2 })
```

These examples illustrate how to use the `+` function to perform addition for numbers, concatenation for strings/lists, or merging for objects in Pact, facilitating various types of operations.