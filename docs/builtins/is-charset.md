## is-charset
Use `is-charset` to check whether a string conforms to a supported character set. The character sets currently supported are 'CHARSET_LATIN1' (ISO-8859-1) and 'CHARSET_ASCII' (ASCII). Support for additional sets, including those up through ISO 8859-5 supplement, will be added in the future.

### Basic syntax

To check whether a string conforms to a specified character set, use the following syntax:

is-charset *charset input*

### Arguments

Use the following arguments to specify the character set and input string you want to check using the `is-charset` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| charset | integer | Specifies the character set to which the input string should conform. Currently supported values are 'CHARSET_LATIN1' (ISO-8859-1) and 'CHARSET_ASCII' (ASCII). |
| input | string | Specifies the input string to be checked for conformity to the specified character set. |

### Return value

The `is-charset` function returns a boolean value indicating whether the input string conforms to the specified character set.

### Examples

The following examples demonstrate the use of `is-charset` in the Pact REPL:

```lisp
pact>(is-charset CHARSET_ASCII "hello world")
true
```

In this example, the input string "hello world" conforms to the ASCII character set.

```lisp
pact>(is-charset CHARSET_ASCII "I am nÖt ascii")
false
```

In this example, the input string "I am nÖt ascii" contains characters that are not part of the ASCII character set.

```lisp
pact>(is-charset CHARSET_LATIN1 "I am nÖt ascii, but I am latin1!")
true
```

In this example, the input string "I am nÖt ascii, but I am latin1!" conforms to the ISO-8859-1 (Latin-1) character set.
