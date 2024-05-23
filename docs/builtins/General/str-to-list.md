## str-to-list
The `str-to-list` function takes a string `STR` and returns a list where each element is a single-character string.

### Basic syntax

To convert a string into a list of single-character strings, use the following syntax:

`(str-to-list STR)`

### Arguments

Use the following argument to specify the string to be converted into a list of single-character strings using the `str-to-list` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `STR` | `string` | Specifies the string to be converted into a list. |

### Return value

The `str-to-list` function returns a list where each element represents a single character from the input string `STR`.

### Examples

The following examples demonstrate the usage of the `str-to-list` function within a Pact script.

To convert the string "hello" into a list of single-character strings:

```lisp
pact>(str-to-list "hello")
["h", "e", "l", "l", "o"]
```

To concatenate spaces between each character in the string "abcde":

```lisp
(concat (map (+ " ") (str-to-list "abcde")))
" a b c d e"
```

These examples illustrate how to use the `str-to-list` function to convert a string into a list of single-character strings in Pact, which can be useful for various string manipulation tasks.
