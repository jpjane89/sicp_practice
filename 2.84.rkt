#lang racket

(define (raise-level arg n)
  (if (not (raise arg)) n
      (raise-level arg (+ n 1))))

(define (successive-raise arg match)
  (if (= (type-tag (raise arg)) (type-tag (match))) 
      (raise arg)
      (successive-raise (raise arg) match)))


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((a1 (car args))
                    (a2 (cadr args))
                    (level1 (raise-level a1 0))
                    (level2 (raise-level a2 0)))
                (cond ((< a1 a2)
                       (let ((raised-a2 (successive-raise a2 a1))
                          (apply-generic op a1 raised-a2)))
                       (apply-generic op (t1->t2 a1) a2))
                      ((< a2 a1)
                        (let ((raised-a1 (successive-raise a1 a2))
                           (apply-generic op raised-a1 a2))))
                      (else
                         (error "No method for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags))))))