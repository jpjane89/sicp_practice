#lang racket

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        ((and (number? m1) (and (pair? m2) (null? (cdr m2)))) (* m1 (car m2))) 
        (else (list m1 '* m2))))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))

;;;;;;

(define (addend-2 s) (car s))

(define (augend-2 s)
  (cond ((memq '* (cddr s))
        (make-product (caddr s) (cddddr s)))
        (else (caddr s))))
;;;;;;;

(define (multiplier-2 p) (car p))

(define (multiplicand-2 p) (caddr p))

(define (product?-2 x)
  (and (pair? x) (eq? (cadr x) '*) (not (memq '+ (cddr x)))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))


(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base x)
  (cadr x))

(define (exponent x)
  (caddr x))


(define (make-exponentiation e1 e2)
  (cond ((=number? e1 0) 0)
        ((=number? e2 0) 1)
        ((=number? e1 1) 1)
        ((=number? e2 1) e1)
        (else (list '** e1 e2))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend-2 exp) var)
                   (deriv (augend-2 exp) var)))
        ((product?-2 exp)
         (make-sum
           (make-product (multiplier-2 exp)
                         (deriv (multiplicand-2 exp) var))
           (make-product (deriv (multiplier-2 exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp) (make-exponentiation (base exp) (- (exponent exp) 1))) (deriv (base exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))
