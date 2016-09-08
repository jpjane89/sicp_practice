#lang racket

(define right-split (split beside below))
(define up-split (split below beside))

(define (split x y)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller (split x y) painter (- n 1)))
          (x painter (y smaller smaller))))))