#lang racket
(require racket/contract/parametric)

(define (map-accumulater mapping-function acc lst)
  ; fold-acc is a pair consisting of the mapping so far and the "real" accumulator
  (foldr (λ (lst-elem fold-acc)
           (match fold-acc
             [(cons mapping map-acc-acc)
              (let ([mapped-elem (car (mapping-function lst-elem map-acc-acc))]
                    [updated-map-acc-acc (cdr (mapping-function lst-elem map-acc-acc))])
                (cons (cons mapped-elem mapping) updated-map-acc-acc))]))
         (cons (list) acc)
         lst))
(provide
 (contract-out
  [map-accumulater
   (parametric->/c
    [elem-type? acc-type? mapped-elem-type?]
    (->
     (-> elem-type? acc-type? (cons/c mapped-elem-type? acc-type?))
     acc-type?
     (listof elem-type?)
     (cons/c (listof mapped-elem-type?) acc-type?)))]))

(define (map-accumulatel mapping-function acc lst)
  ; fold-acc is a pair consisting of the mapping so far and the "real" accumulator
  (define with-reverse-mapping
    (foldl (λ (lst-elem fold-acc)
             (match fold-acc
               [(cons mapping map-acc-acc)
                (let ([mapped-elem (car (mapping-function lst-elem map-acc-acc))]
                      [updated-map-acc-acc (cdr (mapping-function lst-elem map-acc-acc))])
                  (cons (cons mapped-elem mapping) updated-map-acc-acc))]))
           (cons (list) acc)
           lst))
  (cons (reverse (car with-reverse-mapping)) (cdr with-reverse-mapping)))
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
