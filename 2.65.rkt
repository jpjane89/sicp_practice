#lang racket
(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (union-set tree-set1 tree-set2)
  (cond ((null? tree-set1) tree-set2)
        ((null? tree-set2) tree-set1)
        ((and (null? tree-set1) (null? tree-set2)) '())
        ((= (entry tree-set1) (entry tree-set2) (make-tree (entry tree-set1) (union-set (left-branch tree-set1) (left-branch tree-set2)) (union-set (right-branch tree-set1) (right-branch tree-set2)))))
        ((> (entry tree-set1) (entry tree-set2) (make-tree (entry tree-set1) (union-set (tree-set2) (left-branch tree-set1)) (right-branch tree-set1))))
        ((< (entry tree-set1) (entry tree-set2) (make-tree (entry tree-set2) (union-set (tree-set1) (left-branch tree-set2)) (right-branch tree-set2))))))

(define (intersection-set tree-set1 tree-set2)
  (cond ((or (null? tree-set1) (null? tree-set2)) '())
        ((= (entry tree-set1) (entry tree-set2) (make-tree (entry tree-set1) (intersection-set (left-branch tree-set1) (left-branch tree-set2)) (intersection-set (right-branch tree-set1) (right-branch tree-set2)))))
        ((> (entry tree-set1) (entry tree-set2) (intersection-set1 (tree-set2) (left-branch tree-set1)) (right-branch tree-set1))))
        ((< (entry tree-set1) (entry tree-set2) (make-tree (entry tree-set2) (union-set (tree-set1) (left-branch tree-set2)) (right-branch tree-set2))))))

