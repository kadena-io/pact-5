## enforce-pact-version

Use `enforce-pact-version` to enforce the runtime Pact version to be within a specified range, where the version is greater than or equal to the `min-version` argument and less than or equal to the `max-version` argument. 
The `max-version` argument is optional.

Version values are matched numerically from the left, meaning versions like '2', '2.2', and '2.2.3' would all allow '2.2.3'.

### Basic syntax

To enforce the runtime Pact version within a specified range, use the following syntax:

```pact
(enforce-pact-version min-version max-version)
```

### Arguments

Use the following arguments to specify the minimum and maximum Pact versions for the `enforce-pact-version` Pact function:

| Argument    | Type   | Description                                                    |
|-------------|--------|----------------------------------------------------------------|
| `min-version` | string | Specifies the minimum Pact version to enforce. |
| `max-version` | string | Specifies the maximum Pact version to enforce (optional). |

### Return values

The `enforce-pact-version` function returns a boolean value indicating whether the runtime Pact version is within the specified range.

### Examples

The following example demonstrates how to use the `enforce-pact-version` function to ensure that the runtime Pact version is at least "4.10":

```pact
pact>(enforce-pact-version "4.10")
true
```

If the current Pact version is "4.10" or higher, the function returns `true`. 
If the Pact version is lower than "4.10" or you specify an invalid version, the function fails.

```pact
<interactive>:0:0:Error: Invalid pact version 4.10, minimum allowed: 4.4
```

You can also specify a maximum Pact version:

```pact
pact>(enforce-pact-version "4.0" "4.10")
true
```
