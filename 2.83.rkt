#lang racket

(define (raise z1) 
  (apply-generic 'raise z1))

(put 'raise '(integer) 
     (lambda (x) ((get-coercion 'integer 'rational) x)))

(put 'raise '(rational) 
     (lambda (x) ((get-coercion 'rational 'real) x)))

(put 'raise '(real)
     (lambda (x) ((get-coercion 'real 'complex) x)))

(define (integer->rational n) (cons 'rational (cons n 1)))

(define (rational->real n) (cons 'real (/ (+ (numer n) 0.0) (+ (denom n) 0.0))))
  
(define (real->complex n) (cons 'complex (cons n 0)))
  
(put-coercion 'integer 'rational integer->rational)

(put-coercion 'rational 'real rational->real)

(put-coercion 'real 'complex real->complex)