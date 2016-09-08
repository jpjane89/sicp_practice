#lang racket
'(1 3 (5 7) 9)
(car (cdr (car (cdr (cdr l)))))

'((7))
(car (car l))

'(1 (2 (3 (4 (5 (6 7))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l))))))))))))