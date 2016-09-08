#lang racket

(define (map proc items)
  (if (null? items)
      null
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (square-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree)) (square-tree (cdr tree))))))

(define (square-tree-map tree)
  (map (lambda (tree)
         (if (pair? tree) (square-tree-map tree) (* tree tree))) tree))