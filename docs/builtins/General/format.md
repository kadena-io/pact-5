## format

Use `format` to interpolate specifies `variables` into a `template` string using curly braces (`{}`) for template placeholders.

### Basic syntax

To interpolate variables into a template string using `{}` placeholders, use the following syntax:

```pact
(format template variables)
```

### Arguments

Use the following arguments to specify the template string and variables for the `format` Pact function:

| Argument | Type | Description |
|----------|------|-------------|
| `template` | string | Specifies the template string with `{}` placeholders. |
| `variables` | [any] | Specifies the variables to interpolate into the template. |

### Return values

The `format` function returns a new string with the variables interpolated into the template.

### Examples

The following example demonstrates how to use the `format` function with  `"My {} has {}"` as the template string with two `{}` placeholders:

```pact
pact> (format "My {} has {}" ["dog" "fleas"])
"My dog has fleas"
```

In this example, the `format` function replaces the placeholders in the template string with the specified variables `["dog", "fleas"]` to return the result `"My dog has fleas"`. 

The `format` function is often used to create dynamic strings with variables inserted into specific locations in Pact contracts.
