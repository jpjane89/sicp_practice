#lang racket

(define (adjoin-element x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        ((> x (car set)) (cons (car set) (adjoin-element x (cdr set))))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((and (null? set1) (null? set2)) '())
        (else (union-set (cdr set1) (adjoin-element (car set1) set2)))))