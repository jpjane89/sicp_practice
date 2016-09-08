#lang racket
(magnitude z)
(apply-generic 'magnitude z)
(get 'magnitude 'complex) --> magnitude
(magnitude (contents z))
(apply-generic 'magnitude (contents z))
(get 'magnitude 'polar) --> specific magnitude
(specific magnitude (contents (contents z)))
