## format

Use `format` to input specified `variables` into a `template` string in the location specified using curly braces (`{}`) as template placeholders.

### Basic syntax

To input variables into a template string where specified by the `{}` placeholders, use the following syntax:

```pact
(format template variables)
```

### Arguments

Use the following arguments to specify the template string and variables for the `format` Pact function:

| Argument  | Type        | Description                                      |
|-----------|-------------|--------------------------------------------------|
| `template` | string | Specifies the template string with `{}` placeholders. |
| `variables` | [any] | Specifies the variables to insert into the template placeholders. |

### Return value

The `format` function returns a new string with the values from the specified variables replacing the placeholder curly braces `{}` from the template string.

### Examples

The following example demonstrates how to use the `format` function with  `"My {} has {}"` as the template string with two `{}` placeholders:

```pact
pact> (format "My {} has {}" ["dog" "fleas"])
"My dog has fleas"
```

In this example, the `format` function replaces the placeholders in the template string with the specified values `["dog", "fleas"]` to return the result `"My dog has fleas"`. 

The `format` function is often used to create dynamic strings with variables inserted into specific locations in Pact contracts.
For example, you can use the variables to store dynamic values like the current time:

```pact
(enforce
   (>= curr-time release-time)
   (format "Funds locked until {}. Current time: {}" [release-time curr-time]))
```