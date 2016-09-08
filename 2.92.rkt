#lang racket

(define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (let ((o1 (order (variable p1)))
            (o2 (order (variable p2))))
        (if (> o1 o2) (add-poly p1 (convert-var p2 (variable p1)))
                      (add-poly p2 (convert-var p1 (variable p2)))))))


(define (order var)
  (char->integer (string-ref (symbol->string var) 0)))

(define (convert-var p new-var)
  (if (empty-termlist? (term-list p)) (make-poly new-var (make-term 1 0))
      (if (= 'polynomial (tag coeff))
          (add (mult ((convert-var (coeff (first-term (term-list p))) new-var) (make-poly new-var (make-term 0 (make-poly (variable p) (make-term (order (first-term (term-list p))) 1)))))
               (convert-var (make-poly (variable p) (rest-terms (term-list p))) new-var)))
          (add (mult (coeff (first-term (term-list p)))(make-poly new-var (make-term 0 (make-poly (variable p) (make-term (order (first-term (term-list p))) 1)))))
               (convert-var (make-poly (variable p) (rest-terms (term-list p))) new-var)))))
      

; var = (+ x 1) vs. x
; (+ x 1)**3 + (+ x 1)**2
; (* (+ x 1) (+ x 1) (+ x 1))
        
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
  