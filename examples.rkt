#lang racket

(require minikanren)
(require "interro.rkt")

;;;;

(define (pair x xx) (== xx `(,x . ,x)))
(define (sing x out) (== out `(,x)))
(define (emptyo l) (== l '()))

(define (conso head rest out) (== `(,head . ,rest) out))
(define (caro l out) (!? (conso out : l)))
(define (cdro l out) (!? (conso : out l)))
(define (listo l) (!? (conso : : l)))

(define (ino x l) (!? (conde [(caro l x)]
                             [(ino x (cdro l ?))])))
(define (appendo l s ls) (!?
  (conde [(== '() l) (== s ls)]
         [(conso (caro l ?) (appendo (cdro l ?) s ?) ls)])))

;;;;

(run 1 (q) (!? (== (pair q ?) '(1 . 1))))        ; '(1)
(run 1 (q) (!? (== (pair ? q) '(1 . 1))))        ; '(((1 . 1) 1 . 1))
(run 1 (q) (!? (== (sing (sing q ?) ?) '((1))))) ; '(1)

(println "--------")

(run 1 (x y) (conso x y '(1 2 3)))               ; '((1 (2 3)))
(run 1 (q) (caro q 1))                           ; '((1 . _.0))
(run 1 (q) (cdro q '(1 2)))                      ; '((_.0 1 2))
(run 1 (q) (listo q))                            ; '((_.0 . _.1))

(println "--------")

(run 3 (q) (ino q '(1 2 3)))                     ; '(1 2 3)
(run 3 (q) (!? (appendo (emptyo !) q '(1 2 3)))) ; '((2 3) (3) ())
(run 6 (x y) (appendo x y '(1 2 3 4 5)))
; '( (() (1 2 3 4 5)) 
;    ((1) (2 3 4 5)) 
;    ((1 2) (3 4 5)) 
;    ((1 2 3) (4 5)) 
;    ((1 2 3 4) (5)) 
;    ((1 2 3 4 5) ()) )






