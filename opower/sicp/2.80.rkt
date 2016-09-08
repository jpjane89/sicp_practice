#lang racket

(define (=zero? z1) 
  (apply-generic '=zero? z1))

(put '=zero? '(complex) =zero?)

(put '=zero '(rational) 
     (lambda (x) (= 0 (numer x))))

(put '=zero? '(rectangle) 
     (lambda (x) (and (= 0 (real-part x)) (= 0 (imag-part x)))))

(put '=zero? '(polar)
     (lambda (x) (= 0 (magnitude x))))

(put '=zero '(scheme-number)
     (lambda (x) (= 0 x)))
