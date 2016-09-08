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

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))