#lang racket

(require rackunit)
(require "aliquotseqs.rkt")

(define hard-limit 5)

(test-case "Aliquot sequence eqality tests"
  (check-equal? (compute-sequence 8 hard-limit)
                '(8 7 1)
                "Test deficient number")
  (check-equal? (compute-sequence 24 hard-limit)
                '(24 36 55 17 1)
                "Test abundant number")
  (check-equal? (compute-sequence 6 hard-limit)
                '(6 6 6 6 6)
                "Test perfect number")
  (check-equal? (compute-sequence 95 hard-limit)
                '(95 25 6 6 6)
                "Test aspiring number")
  (check-equal? (compute-sequence 220 hard-limit)
                '(220 284 220 284 220)
                "Test amicable number")
  (check-equal? (compute-sequence 1264460 hard-limit)
                '(1264460 1547860 1727636 1305184 1264460)
                "Test sociable number"))
