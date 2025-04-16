## try

Use the `try` function to attempt a **pure** action, returning a default value in the case of failure. 
Pure functions and pure expressions perform operations that produce a resulting value with no side effects. 

In most programming languages, pure functions and expressions always return identical results for identical arguments and don't allow mutable variables or input and output operations.

In Pact, you can use the `try` built-in function to read from database tables and there's no guarantee that a value in a table remains that same between two calls of the same pure action.
You can't use the `try` built-in function to write to  database tables.

### Basic syntax

To attempt a pure action and return a default value if the action fails, use the following syntax:

```pact
(try default action)
```

### Arguments

Use the following arguments to specify the default value and the action to be attempted using the `try` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `default` | any | Specifies the default value to be returned if the action fails. |
| `action` | any | Specifies the action to be attempted. |

### Return value

The `try` function returns the result of the attempted action. 
If the action fails, it returns the specified default value.

### Examples

The following example demonstrates how to use the `try` function in the Pact REPL. 
This example attempts to use the `enforce` function to specify a condition.
If the condition fails, the `try` function returns the default value `3`:

```pact
pact> (try 3 (enforce (= 1 2) "this will definitely fail"))
3
```

If the `enforce` function specifies a condition that succeeds, the `try` function returns the result.
For example, if the condition succeeds, the result is `true`:

```pact
(try 3 (enforce (= 1 1) "this will definitely fail"))
true
```

In the following example, the default value is a string:

```pact
(try "this enforce fails" (enforce (= 2 1) "this will definitely fail"))
"this enforce fails"
```