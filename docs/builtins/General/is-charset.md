## is-charset

Use `is-charset` to check whether a string conforms to a supported character set.
The character sets currently supported are `CHARSET_LATIN1` (ISO-8859-1) and `CHARSET_ASCII` (ASCII).

### Basic syntax

To check whether a string conforms to a specified character set, use the following syntax:

```pact
(is-charset charset input)
```

### Arguments

Use the following arguments to specify the character set and input string you want to check using the `is-charset` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `charset` | integer or constant | Specifies the character set the input string should conform to. The valid values are `0` or `CHARSET_ASCII` for Latin1 (ISO-8859-1) and `1` or `CHARSET_LATIN1` for ASCII. |
| `input` | string | Specifies the input string to be checked for conformity to the specified character set. |

### Return value

The `is-charset` function returns a boolean value indicating whether the input string conforms to the specified character set.

### Examples

The following example demonstrates how to use the `is-charset` function to check whether the input string "hello world" conforms to the ASCII character set:

```pact
pact> (is-charset CHARSET_ASCII "hello world")
true
```

The following example checks an input string that contains characters that are not part of the ASCII character set:

```pact
pact> (is-charset CHARSET_ASCII "I am nÖt ascii")
false
```

Because the input string contains characters that aren't part of the ASCII character set, the `is-charset` function returns false.
If you check whether the input string "I am nÖt ascii, but I am latin1!" conforms to the ISO-8859-1 (Latin-1) character set, the `is-charset` function returns true.

```pact
pact> (is-charset CHARSET_LATIN1 "I am nÖt ascii, but I am latin1!")
true
```

