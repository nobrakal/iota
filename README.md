# Syntax

NB:
 * `{x}` is a list of `x`
 * `{x|c}` is a list of `x` seperated by character `c`

```
upper_lit ::= (A-Z) [a-Z]*

lower_lit ::= [a-z] [a-Z]*

predicate ::=
  | Link(lower_lit,lower_lit)
  | Has(lower_lit)
  | upper_lit (lower_lit)

formula ::=
  | predicate
  | +predicate
  | formula && formula
  | formula || formula
  | not formula
  | ( formula )

safe ::=
  | formula
  | lower_lit
  | safe && safe
  | safe || safe
  | forall lower_lit formula -> safe
  | exists lower_lit formula && safe
  | safe safe
  | ( safe )

letdef ::=
  LET lower_lit {lower_lit} = safe

general ::=
  | formula
  | formula -> formula

program ::=
  | {letdef} IN {safe | ;} ENSURE {general | ;} MAINTAIN {general | ;}
```

# How it works ?

The compiler (in `lib/main.ml`) works with the following steps:

1. Parsing
2. Typechecking
3. Inlining of all definitions
4. Verification of the structure
