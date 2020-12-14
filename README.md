# interroKanren!?

This defines 3 special symbols in miniKanren using gross macros. It's easiest to explain this with examples. Let `(< x y)` iff "x < y" and `(+ x y z)` iff "x + y == z".

- `:`
  - ex: `(< : 3)` can be treated as the claim that some otherwise irrelevant value is less than 3.
  - In general, `(R ... : ...)` expands to `(fresh (g) (R ... g ...))` with g some fresh symbol.
- `?`
  - ex: `(+ 2 3 ?)` can be treated like the value 5.
  - In general, `(R ... (S ... ? ...) ...)` expands to `(fresh (g) (S ... g ...) (R ... g ...))`.
- `!`
  - ex: `(+ 2 3 !)` can be treated like some value other than 5.
  - In general, `(R ... (S ... ! ...) ...)` expands to `(fresh (g1 g2) (=/= g1 g2) (S ... g1 ...) (R ... g2 ...))`
  
`:` is simple and nice, but `?` seems a little strange. The main idea is that it allows us to recover a functional code-style in a relational language. If `R` is a function-like relation whose last argument represents the output and other arguments represent inputs, it can be used functionally like `(R x1 ... xn ?)`. The `?` can be used in other argument positions, so for example `(+ x ? z)` acts like subtraction. The `!` symbol seemed like it belongs here too for duality.

Note that currently the symbols only work inside a `(!? ...)` block.

### Example

We can define `appendo` in a way that looks just like [the functional version](http://io.livecode.ch/learn/gregr/icfp2017-artifact-auas7pp#relational-programming-in-minikanren):

```racket
(define (conso head rest out) (== `(,head . ,rest) out))
(define (caro l out) (!? (conso out : l)))
(define (cdro l out) (!? (conso : out l)))

(define (appendo l s ls) (!?
  (conde [(== '() l) (== s ls)]
         [(conso (caro l ?) (appendo (cdro l ?) s ?) ls)])))

(run 6 (x y) (appendo x y '(1 2 3 4 5)))
```

Gives

```racket
'( (() (1 2 3 4 5)) 
   ((1) (2 3 4 5)) 
   ((1 2) (3 4 5)) 
   ((1 2 3) (4 5)) 
   ((1 2 3 4) (5)) 
   ((1 2 3 4 5) ()) )
```

### TODO

- make neater macros like `run!?`, `define!?`
- make run* not loop
- preserve syntax info
