A Simple Programming Lanuage compiler.

# Type check example:

```
a = Lexer.lexDet "test" "1;"
b = SPLParser.parseDet' SPLParser.pExpression a
Checker.runTInf (Checker.typeInferenceExpr Checker.emptyMap b)

> (Right TInt,TInfState {tInfSupply = 0, tInfSubst = fromList []})
```
