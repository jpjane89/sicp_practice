#lang racket
(define (make-interval a b) (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(define (make-center-percent center percent)
  (make-interval (- center (* center percent))(+ center (* center percent))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (percent interval)
  (/(- (upper-bound interval) (center interval))(center interval)))