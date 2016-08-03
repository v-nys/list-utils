#lang racket
(require rackunit "utils.rkt")
(check-equal? (mapAccum (λ (e acc) (cons (+ e acc) (- (+ e acc) 1))) 1 '()) '() "Sum of element and accumulator, with accumulator = sum - 1, empty list")
(check-equal? (mapAccum (λ (e acc) (cons (+ e acc) (- (+ e acc) 1))) 1 '(1 2 3 4)) '(2 3 5 8) "Sum of element and accumulator, with accumulator = sum - 1, nonempty list")