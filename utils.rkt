#lang racket
(require racket/contract/parametric)
; note: this works, but it is non-tail-recursive
; will leave it this way for now - if there is a problem in an application using this, I'll rewrite it
; TODO use right-associative version as a starting point (but consing is more expensive, maybre reverse result at the end?)
(define (map-accumulatel mapping-function acc lst)
  (match lst [(list) (cons lst acc)]
    [(list-rest h t) (let* ([mapped (mapping-function h acc)]
                            [mapped-val (car mapped)]
                            [mapped-acc (cdr mapped)]
                            [rec-call (map-accumulatel mapping-function mapped-acc t)])
                       (cons (cons mapped-val (car rec-call)) (cdr rec-call)))]))

(provide
 (contract-out
  [map-accumulatel
   (parametric->/c
    [elem-type? acc-type? mapped-elem-type?]
    (->
     (-> elem-type? acc-type? (cons/c mapped-elem-type? acc-type?))
     acc-type?
     (listof elem-type?)
     (cons/c (listof mapped-elem-type?) acc-type?)))]))


(define (map-accumulater mapping-function acc lst)
  (foldr (λ (lst-elem fold-acc)
           (match fold-acc
             [(cons mapping map-acc-acc)
              (let ([mapped-elem (car (mapping-function lst-elem map-acc-acc))]
                    [updated-map-acc-acc (cdr (mapping-function lst-elem map-acc-acc))])
                (cons (cons mapped-elem mapping) updated-map-acc-acc))]))))
              ; TODO add contract!
(provide map-accumulater)