#lang racket
#lang racket

(define (=zero? z1) 
  (apply-generic '=zero? z1))

(put '=zero? '(polynomial) check-coefficients)
       
(define (check-coefficients x)
  (cond ((null? (car x)) true)
        ((not (=zero? (first-term x))) false)
        ((=zero? (first-term x)) (check-coefficients (rest-terms x)))))