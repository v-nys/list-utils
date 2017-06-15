; MIT License
; 
; Copyright (c) 2016 Vincent Nys
; 
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:
; 
; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.
; 
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.

#lang at-exp racket

(require racket/contract/parametric)

(require scribble/srcdoc)
(require (for-doc scribble/manual))

(define (map-accumulater mapping-function acc lst)
  ; fold-acc is a pair consisting of the mapping so far and the "real" accumulator
  (foldr (λ (lst-elem fold-acc)
           (match fold-acc
             [(cons mapping map-acc-acc)
              (let* ([mapped-pair (mapping-function lst-elem map-acc-acc)]
                     [mapped-elem (car mapped-pair)]
                     [updated-map-acc-acc (cdr mapped-pair)])
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
                (let* ([mapped-pair (mapping-function lst-elem map-acc-acc)]
                       [mapped-elem (car mapped-pair)]
                       [updated-map-acc-acc (cdr mapped-pair)])
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

(define (findf-index proc lst)
  (define (findf-index-aux proc lst index)
    (cond [(null? lst) #f]
          [(proc (car lst)) index]
          [else (findf-index-aux proc (cdr lst) (+ index 1))]))
  (findf-index-aux proc lst 0))
(provide
 (contract-out
  [findf-index (-> (-> any/c boolean?) list? (or/c #f exact-nonnegative-integer?))]))

(define (odd-elems lst)
  (reverse
   (cdr
    (foldl (λ (elem acc) (if (car acc) (cons #f (cons elem (cdr acc))) (cons #t (cdr acc))))
           (list #t)
           lst))))
(provide
 (contract-out
  [odd-elems (-> list? list?)]))

(define (group-by proc lst)
  (reverse
   (foldl
    (λ (elem acc)
      (let* ([elem-outcome (proc elem)]
             [outcome-group (findf-index (λ (g) (equal? (proc (car g)) elem-outcome)) acc)])
        (if outcome-group
            (append
             (take acc outcome-group)
             (list (append (list-ref acc outcome-group) (list elem)))
             (drop acc (+ outcome-group 1)))
            (cons (list elem) acc))))
    '()
    lst)))
(provide
 (proc-doc/names
  group-by
  (-> (-> any/c any/c) list? list?)
  (proc lst)
  @{Splits a list @racket[lst] into sublists such that all elements in a sublist
 have the same result for @racket[proc] (based on @racket[equal?]).}))

(define (all-splits-on pred? lst)
  (foldl
   (λ (elem idx acc)
     (if (not (pred? elem))
         acc
         (cons (list (take lst idx) elem (drop lst (+ idx 1))) acc)))
   (list)
   lst
   (range 0 (length lst))))
(provide
 (proc-doc/names
  all-splits-on
  (-> (-> any/c boolean?) list? (listof list?))
  (pred lst)
  @{Computes all possible splits of @racket[lst] on a supplied predicate @racket[pred].}))

(define (subsequences lst)
  (match lst
    [(list) (list)]
    [(list val) (list lst)]
    [(list-rest val vals)
     (append
      (map (λ (num) (drop-right lst num)) (range (length lst)))
      (subsequences vals))]))
(module+ test
  (require rackunit)
  (check-equal?
   (subsequences '(1))
   '((1)))
  (check-equal?
   (subsequences '(1 2 3 4))
   '((1 2 3 4) (1 2 3) (1 2) (1) (2 3 4) (2 3) (2) (3 4) (3) (4))))
(provide
 (proc-doc/names
  subsequences
  (-> list? (listof list?))
  (lst)
  @{Returns a list of all nonempty subsequences of @racket[lst].}))
  