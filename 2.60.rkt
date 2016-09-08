#lang racket

;;; element-of-set?
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))
;;; makes no difference

;;; adjoin-set
(define (adjoin-set x set)
   (cons x set))
;;; more efficient

;;; intersection-set
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))
;;; makes no difference

;;; union-set
(define (union-set set1 set2)
  (cond ((and (null? set1) (null? set2)) '())
        ((null? set1) set2)
        ((null? set2) set1)
        (else (union-set (cdr set1) (cons (car set1) set2)))))
;;; more efficient

; if you are joining elements it makes sense, if you need to find overlap it makes no difference