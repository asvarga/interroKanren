#lang racket

(require minikanren)
(require (for-syntax racket/match racket/block))

(provide !?)

;;;;

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
         
(define-for-syntax (!?-helper t)
  (if (not (list? t)) t
      (let* ([freshes (box null)]
             [conds (box null)]
             [mapped (for/list ([x t]) (mk-value x freshes conds))]
             [ret (if (null? (unbox freshes))
                      (mk-prop (map !?-helper t))
                      `(fresh ,(unbox freshes) ,@(map !?-helper (unbox conds)) ,(!?-helper mapped)))])
        ;(println ret)
        ret)))
(define-syntax (!? syn) (datum->syntax syn (!?-helper (cadr (syntax->datum syn)))))
;(define-syntax-rule (run!? n qs t) (run n  qs (!? t)))
;(define-syntax-rule (run*!?  qs t) (run #f qs (!? t)))
;(define-syntax-rule (define!? t def) (define t (!? def)))












  