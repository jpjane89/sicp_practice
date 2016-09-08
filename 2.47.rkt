#lang racket

(define (make-frame1 origin edge1 edge2)
  (list origin edge1 edge2))

(define (get-origin1 frame)
  (car frame))

(define (get-edge1-1 frame)
  (car (cdr frame)))

(define (get-edge2-1 frame)
  (car (cdr (cdr frame))))

(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (get-origin2 frame)
  (car frame))

(define (get-edge1-2 frame)
  (car (cdr frame)))

(define (get-edge2-2 frame)
  (cdr (cdr frame)))