# signalHexag

# language Quick Syntax

```
<define> ::=
  | (define <symbol> <args> <expr>)
  | (define <symbol> <expr>)

<expr> ::=
  | (<symbol> <expr>)
  | (let [[<symbol> <expr>] .... ] <expr>)
  | <atom>
  | <list>
  | (if <expr> <expr> <expr>)

<list> ::= 
  | [<atom> | <atom> ]
  | [<atom> .... <atom> ]
  | [<atom> .... <atom> | <atom> ]
      
<atom> ::=
  | <symbol>
  | <number>
  | <string>
```
