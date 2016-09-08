#lang racket

(define (project z1) 
  (apply-generic 'project z1))

(put 'project '(complex) 
     (lambda (x) (car x)))

(put 'project '(real) 
     (lambda (x) (round x)))

(put 'project '(rational)
     (lambda (x) (round (/ (+ (numer n) 0.0) (+ (denom n) 0.0)))))

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

(define (drop z)
  (if (equ? (raise (project z)) z) (drop (raise (project z)))
      z))
