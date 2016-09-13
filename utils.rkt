#lang racket
; note: this works, but it is non-tail-recursive
; will leave it this way for now - if there is a recursion depth problem in an application using this, I'll rewrite it
(define (mapAccum mapping-function acc lst)
  (match lst [(list) (cons lst acc)]
    [(list-rest h t) (let* ([mapped (mapping-function h acc)]
                            [mapped-val (car mapped)]
                            [mapped-acc (cdr mapped)]
                            [rec-call (mapAccum mapping-function mapped-acc t)])
                       (cons (cons mapped-val (car rec-call)) (cdr rec-call)))]))
(provide mapAccum)