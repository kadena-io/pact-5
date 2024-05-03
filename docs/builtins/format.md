Use `format` to interpolate variables `VARS` into a `TEMPLATE` string using `{}` placeholders.

### Basic syntax

To interpolate variables into a template string using `{}` placeholders, use the following syntax:

format *template* *vars* -> *result*

### Arguments

Use the following arguments to specify the template string and variables for the `format` Pact function:

| Argument  | Type        | Description                                      |
|-----------|-------------|--------------------------------------------------|
| template  | string      | Specifies the template string with `{}` placeholders. |
| vars      | [*]         | Specifies the variables to interpolate into the template.    |

### Return values

The `format` function returns a new string with the variables interpolated into the template.

### Examples

The following example demonstrates the `format` function:

```lisp
(format "My {} has {}" ["dog" "fleas"])
```

In this example, `"My {} has {}"` is the template string with two `{}` placeholders. The `format` function is used to interpolate the variables `["dog", "fleas"]` into the template. The result of this interpolation is `"My dog has fleas"`. The `format` function provides a way to create dynamic strings with variables inserted into specific locations in Pact contracts.
