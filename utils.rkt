#lang typed/racket
(: mapAccum (All (ElemType AccType OutType) (-> (-> ElemType AccType (Pair OutType AccType)) AccType (Listof ElemType))))
(define (mapAccum mapping-function acc lst)
  (match lst [(list) lst]
    [(list-rest h t) (let* ([mapped (mapping-function h acc)]
                            [mapped-val (car mapped)]
                            [mapped-acc (cdr mapped)])
                       (cons mapped-val (mapAccum mapping-function mapped-acc t)))]))