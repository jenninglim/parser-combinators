> import Parser

Expression
==========

Consider the following (extended) BNF for expressions:

 < Num >   ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
 < AExpr > ::= < Num > "+" < AExpr > | < Num > "-" < AExpr >

Note that the recursion specified by the BNF is guarded.
We will create a datatype that corresponds to the non-terminals:

> data Num   = Num Int
> data AExpr = Add Num AExpr
>            | Sub Num AExpr

We will now create its corresponding parsers.


