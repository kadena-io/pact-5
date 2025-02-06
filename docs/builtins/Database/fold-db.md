## fold-db

Use `fold-db` to select rows from a specified `table` using a predicate `query` with both a key and a value, and then accumulate the results of the query using a `consumer` function. 
The output is sorted by the ordering of keys.

### Basic syntax

To select rows from a table, apply a predicate, and accumulate the results using a consumer function, use the following syntax:

```pact
(fold-db table query consumer)
```

### Arguments

Use the following arguments to specify the table, predicate, and consumer function for the `fold-db` Pact function:

| Argument  | Type               | Description                                                      |
|-----------|--------------------|------------------------------------------------------------------|
| `table` | `table: <{row}>` | Specifies the table from which to select rows. |
| `query` | `a:string b:object: <{row}>` | Specifies the predicate function to apply to each row and return a boolean value. |
| `consumer` | function with `key: string` and `value: object: <{row}>` | Specifies the consumer function used to accumulate results from each row and return the final result from all accumulated results. |

### Return value

The `fold-db` function returns a list of accumulated results based on the predicate `query` and the `consumer` function.

### Examples

The following example demonstrates how to use the `fold-db` function:

```pact
(let
  ((query (lambda (k obj) true)) ;; Select all rows
   (func (lambda (x) [(at 'firstName x), (at 'b x)])) ;; Example consumer function
  )
  (fold-db people query func)
)
```

In this example:
- `(qry (lambda (k obj) true))` is a predicate that selects all rows.
- `(f (lambda (x) [(at 'firstName x), (at 'b x)]))` is a consumer function that selects the "firstName" and "b" fields from each row.

The `fold-db` function is then used to select rows from the `people` table using the predicate `qry` and accumulate the results using the consumer function `f`. The result is a list of accumulated results based on the selected rows and the specified consumer function. The `fold-db` function is useful for iterating over rows in a table and performing operations in Pact contracts.
