## expect

Use `expect` to evaluate an expression and verify that the result equals an expected value.

### Basic syntax

To evaluate an expression that returns an expected result, use the following syntax:

```pact
(expect doc expected actual)
```

### Arguments

Use the following arguments when using the `expect` Pact function.

| Argument | Type | Description |
|----------|------|-------------|
| `doc` | string | Specifies the documentation string describing the expectation.               |
| `expected` | any | Specifies the expected value to compare against the result of `actual`.|
| `actual` | any  | Specifies the expression to evaluate. The expression can be of any Pact type. |

### Return value

The `expect` function returns a string indicating the success or failure of the expectation.

### Examples

The following example demonstrates how to use the function `expect` to evaluate an expression that returns an expected result in a Pact REPL:

```pact
pact> (expect "Sanity prevails." 4 (+ 2 2))
"Expect: success: Sanity prevails."
```

The following example illustrates using the `expect` function to verify that the information expected to be inserted into a table is the same as the result of the `get-token-info` function:

(expect "Token info is inserted into table"
    { "id": "t:YV6-cQBhE_EoIXAuNV08aGXLfcucBEGy0Gb1Pj6w_Oo"
     ,"supply": 0.0
     ,"precision": 0
     ,"uri": "test-uri"
     ,"policies": []
    }
    (get-token-info "t:YV6-cQBhE_EoIXAuNV08aGXLfcucBEGy0Gb1Pj6w_Oo")
)
