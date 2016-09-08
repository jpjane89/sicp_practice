#lang racket

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (make-sum cddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (make-sum cddr p))

(define (make-sum a1 a2)
  (cond ((and (number? a1) (number? a2))
             (cond ((=number? a1 0) a2)
                   ((=number? a2 0) a1)
                   (else (+ a1 a2))))
        ((and (number? a1) (null? a2)) a1)
        ((and (number? a1) (pair? a2))(+ a1 (make-sum (car a2) (cdr a2))))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((and (number? m1) (number? m2))
             (cond ((or (=number? m1 0) (=number? m2 0)) 0)
                   ((=number? m1 1) m2)
                   ((=number? m2 1) m1)
                   (else (* m1 m2))))
        ((and (number? m1) (null? m2)) m1)
        ((and (number? m1) (pair? m2))(* m1 (make-product (car m2) (cdr m2))))
        (else (list '* m1 m2))))

(x + 3 * (x + y + 2))