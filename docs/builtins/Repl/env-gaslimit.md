## env-gaslimit

_limit_&nbsp;`integer` _&rarr;_&nbsp;`string`

Set environment gas limit to LIMIT.

## env-gaslog

_&rarr;_&nbsp;`string`

Enable and obtain gas logging. Bracket around the code whose gas logs you want to inspect.

```bash
pact> (env-gasmodel "table") (env-gaslimit 10) (env-gaslog) (map (+ 1) [1 2 3]) (env-gaslog)
["TOTAL: 7" "map:GUnreduced:currTotalGas=4: 4" "+:GUnreduced:currTotalGas=5: 1" ":GIntegerOpCost:(1, ):(1, ):currTotalGas=5: 0" "+:GUnreduced:currTotalGas=6: 1" ":GIntegerOpCost:(1, ):(2, ):currTotalGas=6: 0" "+:GUnreduced:currTotalGas=7: 1" ":GIntegerOpCost:(1, ):(3, ):currTotalGas=7: 0"]
```
