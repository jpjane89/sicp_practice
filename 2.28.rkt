#lang racket
(define (fringe x)
  (cond ((null? x) null)
        ((not (pair? x)) (list x))
        (else (append (fringe (car x))
                 (fringe (cdr x))))))