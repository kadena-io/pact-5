## env-gaslog

Use `env-gaslog` to enable gas logging for a block of code.
You can use this function to check the gas required to execute a specific block of code.
  
### Basic syntax

To enable gas logging for a specific block of `code`, use the following syntax:

```pact
(env-gaslog) code (env-gaslog)
```

### Arguments

The `env-gaslog` function does not take any arguments.

### Return value

The `env-gaslog` function returns the log messages in strings that describe the gas consumed by each operation in the code block.

### Examples

The following example demonstrates how to use the `env-gaslog` function to report the gas consumed for the specified `map` function:

```pact
(env-gasmodel "table") (env-gaslimit 10) (env-gaslog) (map (+ 1) [1 2 3]) (env-gaslog)
"Set gas model to table-based cost model"
"Set gas limit to 10"
["TOTAL: 0"]
[2 3 4]
["TOTAL: 7"
"map:GUnreduced:currTotalGas=4: 4"
"+:GUnreduced:currTotalGas=5: 1"
":GIntegerOpCost:(1, ):(1, ):Pact48IntThreshold:currTotalGas=5: 0"
"+:GUnreduced:currTotalGas=6: 1"
":GIntegerOpCost:(1, ):(2, ):Pact48IntThreshold:currTotalGas=6: 0"
"+:GUnreduced:currTotalGas=7: 1"
":GIntegerOpCost:(1, ):(3, ):Pact48IntThreshold:currTotalGas=7: 0"]
```

In this example, the `["TOTAL: 0"]` message signals the beginning of gas logging for the `map` function.
The messages after `"TOTAL: 7"` provide details about how the gas was calculated.
