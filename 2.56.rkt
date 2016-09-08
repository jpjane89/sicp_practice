#lang racket
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp) (make-exponentiation (base exp) (- (exponent exp) 1))) (deriv (base exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base x)
  (cadr x))

(define (exponent x)
  (caddr x))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-exponentiation e1 e2)
  (cond ((=number? e1 0) 0)
        ((=number? e2 0) 1)
        ((=number? e1 1) 1)
        ((=number? e2 1) e1)
        (else (list '** e1 e2))))