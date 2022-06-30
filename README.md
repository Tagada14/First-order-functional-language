# First order functional language
 
 Very simple first order functional language with the following syntax:

p ::= {define {d1 . . . dk} for e}
d := [fun f (x1 . . . xl) = e]
e ::= 
* n
* x
* {e1 ⊕ e2}
* {ifz e0 then e1 else e2}
* {let x be e1 in e2}
* {f (e1 . . . el)}
⊕ ::= + | - | * | <=

Where p is our program that consists of global function definitions (d) that allow us to compute the value of expression e.
