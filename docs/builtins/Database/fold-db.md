## fold-db
Use `fold-db` to select rows from a table `TABLE` using a predicate `QRY` with both key and value, and then accumulate the results of the query using a `CONSUMER` function. The output is sorted by the ordering of keys.

### Basic syntax

To select rows from a table, apply a predicate, and accumulate the results using a consumer function, use the following syntax:

`(fold-db TABLE QRT CONSUMER)`

### Arguments

Use the following arguments to specify the table, predicate, and consumer function for the `fold-db` Pact function:

| Argument  | Type               | Description                                                      |
|-----------|--------------------|------------------------------------------------------------------|
| `TABLE`     | `table:<{row}>`      | Specifies the table from which to select rows.                   |
| `QRY`       | `a:string b:object:<{row}> -> bool` | Specifies the predicate function to apply to each row.          |
| `CONSUMER`  | `a:string b:object:<{row}> -> <b> -> [<b>]`  | Specifies the consumer function to accumulate results.          |

### Return values

The `fold-db` function returns a list of accumulated results based on the predicate `QRY` and the consumer function `CONSUMER`.

### Examples

The following example demonstrates the `fold-db` function:

```pact
(let
  ((qry (lambda (k obj) true)) ;; Select all rows
   (f (lambda (x) [(at 'firstName x), (at 'b x)])) ;; Example consumer function
  )
  (fold-db people qry f)
)
```

In this example:
- `(qry (lambda (k obj) true))` is a predicate that selects all rows.
- `(f (lambda (x) [(at 'firstName x), (at 'b x)]))` is a consumer function that selects the 'firstName' and 'b' fields from each row.

The `fold-db` function is then used to select rows from the `people` table using the predicate `qry` and accumulate the results using the consumer function `f`. The result is a list of accumulated results based on the selected rows and the specified consumer function. The `fold-db` function is useful for iterating over rows in a table and performing operations in Pact contracts.
