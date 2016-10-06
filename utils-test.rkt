#lang racket
(require rackunit "utils.rkt")
(check-equal?
 (map-accumulatel (λ (e acc) (cons (+ e acc) (- (+ e acc) 1))) 1 '())
 (cons '() 1)
 "Sum of element and accumulator, with accumulator = sum - 1, empty list")
(check-equal?
 (map-accumulatel (λ (e acc) (cons (+ e acc) (- (+ e acc) 1))) 1 '(1 2 3 4))
 (cons '(2 3 5 8) 7)
 "Sum of element and accumulator, with accumulator = sum - 1, nonempty list")
(check-equal?
 (map-accumulater (λ (e acc) (cons (+ e acc) (- (+ e acc) 1))) 1 '())
 (cons '() 1)
 "Sum of element and accumulator, with accumulator = sum - 1, empty list")
(check-equal?
 (map-accumulater (λ (e acc) (cons (+ e acc) (- (+ e acc) 1))) 1 '(1 2 3 4))
 (cons '(8 8 7 5) 7)
 "Sum of element and accumulator, with accumulator = sum - 1, nonempty list")

(check-equal? (findf-index (λ (_) #t) '()) #f)
(check-equal? (findf-index odd? '(1 2 3 4 5)) 0)
(check-equal? (findf-index odd? '(2 4 6 1)) 3)
(check-equal? (odd-elems '()) '())
(check-equal? (odd-elems (list 4 9 2 0 7 5 6 7)) '(4 2 7 6)) ; test with even length
(check-equal? (odd-elems (list 9 2 0 7 5 6 7)) '(9 0 5 7)) ; test with odd length