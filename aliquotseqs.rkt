#lang racket

; aliquot sequence
;
; given a number n, sum the factors excluding n, and repeat

(define (sum-list lst)
  (foldl + 0 lst))

(define (aliquot-factors n)
  (filter (lambda (x) (= (modulo n x) 0))
          (range 1 (+ n 1))))

(define (compute-sequence n limit)
  (if (> limit 0)
      (let ([next-n (sum-list
                     (drop-right (aliquot-factors n) 1))])
        (cons n
              (if (> next-n 0)
                  (compute-sequence next-n (- limit 1))
                  '())))
      '()))

(define hard-limit 5)

(compute-sequence 8 hard-limit) ; deficient
(compute-sequence 24 hard-limit) ; abundant
(compute-sequence 6 hard-limit) ; perfect
(compute-sequence 95 hard-limit) ; aspiring
(compute-sequence 220 hard-limit) ; amicable
(compute-sequence 1264460 hard-limit) ; sociable
