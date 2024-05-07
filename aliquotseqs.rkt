#lang racket

(require plot)

(provide compute-sequence)

; aliquot sequence
;
; given a number n, sum the factors excluding n, and repeat

(define (sum-factors lst)
  (foldl + 0 lst))

(define (factors n)
  (drop-right (filter (lambda (x) (= (modulo n x) 0))
                      (range 1 (+ n 1)))
              1))

(define (compute-sequence n limit)
  (if (> limit 0)
      (let ([next-n (sum-factors (factors n))])
        (cons n
              (if (> next-n 0)
                  (compute-sequence next-n (- limit 1))
                  '())))
      '()))

(define (generate-points seq)
  (for/list ([num (in-list seq)] [idx (in-naturals)])
    (list idx num)))

(define (plot-points points)
  (plot (list (lines points #:color 'red #:width 4))))

(define (plotter n limit)
  (plot-points (generate-points (compute-sequence n
                                                  limit))))

(module+ main
  (plot-new-window? #t)
  (plotter 24 5))
