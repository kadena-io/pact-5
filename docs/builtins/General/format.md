## format

Use `format` to input specified variables `vars` into a `template` string using `{}` placeholders.

### Basic syntax

To input variables into a template string using `{}` placeholders, use the following syntax:

```pact
(format template vars)
```

### Arguments

Use the following arguments to specify the template string and variables for the `format` Pact function:

| Argument  | Type        | Description                                      |
|-----------|-------------|--------------------------------------------------|
| `template` | string | Specifies the template string with `{}` placeholders. |
| `vars` | any | Specifies the variables to interpolate into the template. |

### Return values

The `format` function returns a new string with the values from the specified variables replacing the placeholder curly braces `{}` from the template string.

### Examples

The following example demonstrates the `format` function:

```pact
pact>(format "My {} has {}" ["dog" "fleas"])
"My dog has fleas"
```

In this example, `"My {} has {}"` is the template string with two `{}` placeholders. The `format` function is used to interpolate the variables `["dog", "fleas"]` into the template. The result of this interpolation is `"My dog has fleas"`. The `format` function provides a way to create dynamic strings with variables inserted into specific locations in Pact contracts.
