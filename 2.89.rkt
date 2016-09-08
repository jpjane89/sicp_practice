#lang racket

(define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
           (cond ((> (length L1) (length L2))
                  (adjoin-term
                   (first-term L1) (add-terms (rest-terms L1) L2)))
                 ((< (length t1) (length t2))
                  (adjoin-term
                   (first-term L2) (add-terms L1 (rest-terms L2))))
                 (else
                  (adjoin-term
                   (make-term (add (first-term L1) (first-term L2)))
                   (add-terms (rest-terms L1)
                              (rest-terms L2))))))))