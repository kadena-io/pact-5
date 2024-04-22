Use `compose` to compose functions *`x`* and *`y`* such that *`x`* operates on *`value`*, and *`y`* operates on the results of *`x`*.

## Basic syntax

To compose functions *`x`* and *`y`* with *`value`*, use the following syntax:

compose *x* *y* *value*

## Arguments

Use the following arguments to specify the functions *`x`* and *`y`* for composing with *`value`* using the `compose` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| x | x:<a> -> <b> | Specifies the first function *`x`* to operate on *`value`*. |
| y | x:<b> -> <c> | Specifies the second function *`y`* to operate on the results of *`x`*. |
| value | <a> | Specifies the value on which *`x`* operates. |

## Return values

The `compose` function returns the result of applying the composed functions *`x`* and *`y`* on the *`value`*.

## Examples

The following example demonstrates the `compose` function in the Pact REPL:

```lisp
pact>(filter (compose (length) (< 2)) ["my" "dog" "has" "fleas"])
["my" "has"]
```

In this example, `compose` composes the function `(length)` with the predicate function `(< 2)`, such that `(length)` operates on each element of the list, and `(< 2)` operates on the results of `(length)`. The result is a filtered list containing elements with a length less than 2.
