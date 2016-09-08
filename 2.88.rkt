#lang racket

(define (negate x)
  (sub 0 x))

(define (subtract-terms L1 L2)
  (cond ((empty-termlist? L1) (negate L2))
        ((empty-termlist? L2) (negate L1))
        (else
         (let ((t1 (first-term L1)) (t2 (first-term L2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term
                   t1 (subtract-terms (rest-terms L1) L2)))
                 ((< (order t1) (order t2))
                  (adjoin-term
                   ((negate t2) (subtract-terms L1 (rest-terms L2)))))
                 (else
                  (adjoin-term
                   (make-term (order t1)
                              (subtract (coeff t1) (coeff t2)))
                   (subtract-terms (rest-terms L1)
                              (rest-terms L2)))))))))