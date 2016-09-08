#lang racket

(define (equal? list-a list-b)
  (cond ((and (null? list-a) (null? list-b)) true)
        ((or (null? list-a) (null? list-b)) false)
        ((eq? (car list-a) (car list-b)) (equal? (cdr list-a) (cdr list-b)))
         (else false)))