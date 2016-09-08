#lang racket

(define (even? num)
  (= 0 (modulo num 2)))

(define (odd? num)
  (not (= 0 (modulo num 2))))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (loop fcn count desc-list asc-list)
   (if (= 0 count) asc-list
      (if (fcn (list-ref desc-list count)) (loop fcn (- count 1) desc-list (cons (list-ref desc-list count) asc-list)) (loop fcn (- count 1) desc-list asc-list))))
  
(define (same-parity x . y)
  (if (even? x) (loop even? (- (length y) 1) y null)
     (loop odd? (- (length y) 1) y null)))
     
