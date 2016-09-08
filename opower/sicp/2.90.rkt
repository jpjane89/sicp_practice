#lang racket

(define (install-sparse-package)
  
  (define (make-sparse term-list)
    (attach-tag 'sparse term-list))
 
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (attach-tag 'sparse (cons term (cdr term-list)))))
  
  (define (first-term term-list) 
    (car term-list)))
    
(define (install-dense-package)
  
  (define (make-dense term-list)
    (attach-tag 'dense term-list))
  
  (define (adjoin-term term term-list)
    (cond ((=zero? (coeff term)) term-list)
          ((= (+ (order (first-term term-list)) 1) (order term)) (attach-tag 'dense (cons (coeff term) (cdr term-list))))
          (else (adjoin-term term (prepend-zero (- (order (first-term term-list)) (order term)) term-list)))))
  
  (define (prepend-zero num term-list)
    (if (= (num 1)) term-list
          (cons 0 (prepend-zero (- num 1) term-list))))
  
  (define (first-term term-list) 
    (make-term (- (length term-list) 1) (car term-list))))
      
(define (install-polynomial-package)
  
  (define (adjoin-term term term-list)
    (apply-generic 'adjoin-term term term-list))
  
  (define (first-term term-list)
    (apply-generic 'first-term term-list))
  
  (define (the-empty-termlist) '())

  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1)) (t2 (first-term L2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term
                   t1 (add-terms (rest-terms L1) L2)))
                 ((< (order t1) (order t2))
                  (adjoin-term
                   t2 (add-terms L1 (rest-terms L2))))
                 (else
                  (adjoin-term
                   (make-term (order t1)
                              (add (coeff t1) (coeff t2)))
                   (add-terms (rest-terms L1)
                              (rest-terms L2)))))))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
         (make-term (+ (order t1) (order t2))
                    (mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms t1 (rest-terms L)))))))




