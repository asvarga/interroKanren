#lang racket

(require minikanren)
(require minikanren/numbers)
(require (for-syntax racket/match racket/block))

;;;;

;(define (id x) x)

;(define-syntax-rule (@ . t) t)
;(define-syntax-rule (@ . t)
  ;(fresh (_) t 

;(run 1 (q) (@ == q q))
;(run 1 (q) (fresh (x) (pair x q)))
;(run 1 (q) (pair q `(1 . 1)))

;(run 1 (q) (== (@ pair q _) `(1 . 1)) ) ~>
;(run 1 (q) (fresh (_) (pair q _) (== _ `(1 . 1))) )
;(run 1 (q) (@ == ($ pair q _) `(1 . 1)) )

;(== (sing (sing q _) _) `((1)))
;(run 1 (q) (fresh (x y) (sing q x) (sing x y) (== y `((1)))))



;; we only have to worry about shallow single relations like (R ... _ ...)
;; replace (R ... _ ...) with foo, emit (R ... foo ...), wrap with (fresh (foo) ...)
;; this would be easier if we had a primitive like (s.t. (foo) (R ... foo ...))


;(_run 1 (q) (R a (S b _ c) q)) ~>
;(run 1 (q) (fresh (_) (R a _ q) (S b _ c)))

;(define-syntax-rule (_run n qs t)
 ; (run n qs t))
;(define-syntax-rule (_run* qs t) (_run #f qs t))
;(_run 1 (q) (== q q))

;(define-for-syntax (has-hole t) (and (list? t) (member '? t)))
;(define-for-syntax (fill-hole t sym) (for/list ([x t]) (if (equal? x '?) sym x)))
;(define-for-syntax (fill-holes ts syms)
;  (match ts
;    [(cons head rest) (if (has-hole head)
;                          (cons (fill-hole head (car syms)) (fill-holes rest (cdr syms)))
;                          (fill-holes rest syms))]
;    [_ '()]))
;(define-for-syntax (replace ts syms)
;  (match ts
;    [(cons head rest) (if (has-hole head)
;                          (cons (car syms) (replace rest (cdr syms)))
;                          (cons head (replace rest syms)))]
;    [_ '()]))
;(define-for-syntax (funcify t)
;  (if (not (list? t)) t
;      (let* ([holey (filter has-hole t)]
;             [c (length holey)]
;             [syms (build-list c (lambda (x) (gensym '?)))]
;             [filled (fill-holes t syms)]
;             [replaced (replace t syms)]
;             [ret (if (> c 0) `(fresh ,syms ,@(map funcify filled) ,(funcify replaced)) t)])
;        (println ret)
;        ret)))
;(define-syntax (?? syn) (datum->syntax syn (funcify (cadr (syntax->datum syn)))))

(define-for-syntax (cons! x xs) (set-box! xs (cons x (unbox xs))))
(define-for-syntax (mk-value t freshes conds)
  (if (not (list? t)) t
      (block
       (define value #f)
       (define mapped (for/list ([x t])
                        (match x
                          ['? (let ([sym (gensym '?)])
                                (set! value sym)
                                (cons! sym freshes)
                                sym)]
                          ['! (let ([sym (gensym '!)]
                                    [nsym (gensym '!)])
                                (set! value sym)
                                (cons! sym freshes)
                                (cons! nsym freshes)
                                (cons! `(=/= ,sym ,nsym) conds)
                                nsym)]
                          [': (let ([sym (gensym ':)]) (cons! sym freshes) sym)]
                          [_ x])))
       (if value (block (cons! mapped conds) value) mapped))))
(define-for-syntax (mk-prop t)
  (if (not (list? t)) t
      (block
       (define freshes (box null))
       (define mapped (for/list ([x t])
                        (match x
                          [': (let ([sym (gensym ':)]) (cons! sym freshes) sym)]
                          [_ x])))
       (if (null? (unbox freshes)) t `(fresh ,(unbox freshes) ,mapped)))))
         
(define-for-syntax (mk-fresh t)
  (if (not (list? t)) t
      (let* ([freshes (box null)]
             [conds (box null)]
             [mapped (for/list ([x t]) (mk-value x freshes conds))]
             [ret (if (null? (unbox freshes))
                      (mk-prop (map mk-fresh t))
                      `(fresh ,(unbox freshes) ,@(map mk-fresh (unbox conds)) ,(mk-fresh mapped)))])
        ;(when (not (null? (unbox freshes))) (println ret))
        ;(println ret)
        ret)))
(define-syntax (!? syn) (datum->syntax syn (mk-fresh (cadr (syntax->datum syn)))))
;(define-syntax-rule (run!? n qs t) (run n  qs (!? t)))
;(define-syntax-rule (run*!?  qs t) (run #f qs (!? t)))
;(define-syntax-rule (define!? t def) (define t (!? def)))

(define (pair x xx) (== xx `(,x . ,x)))
(define (sing x out) (== out `(,x)))
;(run 1 (q) (!? (== (pair q ?) '(1 . 1))))
;(run 1 (q) (!? (== (pair ? q) '(1 . 1))))
;(run 1 (q) (!? (== (sing (sing q ?) ?) '((1)))))

(define (conso head rest out) (== `(,head . ,rest) out))
(define (caro l out) (!? (conso out : l)))
(define (cdro l out) (!? (conso : out l)))
;(define (cdro l out) (!? (== (cons : l) out)))

(define (appendo l s ls) (!?
  (conde [(== '() l) (== s ls)]
         [(conso (caro l ?) (appendo (cdro l ?) s ?) ls)])))

;(run 6 (x y) (appendo x y '(1 2 3 4 5)))

(define (ino x l) (!? (conde [(caro l x)]
                             [(ino x (cdro l ?))])))
(run 3 (q) (ino q '(1 2 3)))
;(define (incdro x l) (
(define (listo l) (!? (conso : : l)))
(define (emptyo l) (== l '()))
(run 3 (q) (!? (appendo (emptyo !) q '(1 2 3))))

;(run 2 (q) (!? (conde
 ;               [(== (pair 1 ?) q)]
  ;              [(== (sing 1 ?) q)])))

         ;[(!? (conso (conso ? : l) (appendo (conso : ? l) s ?) ls))]))

;(run 1 (q) (appendo '(1 2) '(3 4 5) q))
;(run 1 (q) (!? (conso : q '(1 2 3))))
;(run 1 (q) (!? (conso : q (conso : ? '(1 2 3)))))
        
;; TODO: cleanup
;; TODO: get run!? working
;; TODO: get list creators working











  