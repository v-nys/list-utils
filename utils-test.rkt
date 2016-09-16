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