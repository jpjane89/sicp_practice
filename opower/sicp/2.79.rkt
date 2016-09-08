#lang racket

(define (equ? z1 z2) 
  (apply-generic 'equ? z1 z2))

(put 'equ? '(complex complex) equ?)

(put 'equ? '(rational rational) 
     (lambda (x y) (= (* (numer x) (denom y)) (* (numer y) (denom x)))))

(put 'equ? '(rectangle rectangle) 
     (lambda (x y) (and (= (real-part x) (real-part y))
       (= (imag-part x) (imag-part y)))))

(put 'equ? '(polar polar)
     (lambda (x y) (and (= (magnitude x) (magnitude x))
       (= (angle y) (angle y)))))

(put 'equ? '(scheme-number scheme-number) =)