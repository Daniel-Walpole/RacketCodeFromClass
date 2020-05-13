#lang racket
(define (dupSymSpl L)
  (cond
    [(list? (car L)) (dupSymSpl (first L)) (dupSymSpl (cdr L))]
    [(string? (car L)) '()]
    [(char? (car L))(append L L)]
    )
  )