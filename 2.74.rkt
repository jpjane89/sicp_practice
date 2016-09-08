#lang racket

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

; 1
(define (get-record file employee-name )
  (apply-generic 'get-record file employee-name))

; ('division file)
; ('employee-name record))

; 2
(define (get-salary employee-name file)
  (apply-generic 'get-salary (get-record file employee-name)))

; ('division file)
; ('employee-name record)
; ('salary salary) 
; ('address address)

; 3
(define (find-employee-record employee-name files)
     (cond ((null? files) error "No match")
           ((not (null? (get-record (car files) employee-name))) (get-record (car files) employee-name))
           (else (find-employee-record employee-name (cdr files)))))

; add personnel info to corresponding division file