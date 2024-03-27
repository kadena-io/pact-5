# enumerate

Use `enumerate` to return a sequence of numbers from the specified *`first`* number to the specified *`last`* number, inclusively, as a list. 

By default, the sequence increments by one from the *`first`* number to the *`last`* number. Optionally, you can specify an increment other than one to use between numbers in the sequence. 

If you specify a *`first`* number that’s greater than the *`last`* number, the sequence decrements by one from the *`first`* number to the *`last`* number.

## Basic usage

To increment or decrement the sequence by one, use the following syntax:

enumerate *first* *last*

To specify a value to increment or decrement the sequence by, use the following syntax:

enumerate *first* *last inc*

## Arguments

Use the following arguments to define the beginning and end of the sequence you want to list using the `enumerate` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| first | integer | Specifies the first number in the sequence. |
| last | integer | Specifies the last number in the sequence. |

## Options

Use the following option to define the increment to use between the beginning and end of the sequence in the `enumerate` Pact function.

| Option | Type | Description |
| --- | --- | --- |
| inc | integer | Specifies the increment to use between numbers in the sequence. |

## Return values

The `enumerate` function returns the resulting sequence of numbers as a list.

## Examples

The following example enumerates a sequence of numbers using the default increment of one in the Pact REPL:

```lisp
pact>(enumerate 0 10)
[0 1 2 3 4 5 6 7 8 9 10]
```

The following example enumerates a sequence of numbers using an increment of two between numbers in the sequence:

```lisp
pact>(enumerate 0 10 2)
[0 2 4 6 8 10]
```

The following example illustrates decrementing a sequence of numbers using an `inc` value of -2 between numbers in the sequence:

```lisp
(enumerate 20 10 -2)
[20 18 16 14 12 10]
```