#lang racket
(define (symNumSep l)
  (if (null? l) '()
      (insert (car l)(symNumSep cdr l))
  )
)
(define (insert item l)
  (cond ((null? item) (list l))
        (( > (car l)
             (cons (car l)(insert l (cdr l))))
         (else (cons l item))
   ))
)
  