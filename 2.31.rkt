#lang racket

(define (map proc items)
  (if (null? items)
      null
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (tree-map f tree)
  (map (lambda (tree)
         (if (pair? tree) (tree-map f tree) (f tree))) tree))

(define (square x) (* x x))

(define (square-tree tree) (tree-map square tree))