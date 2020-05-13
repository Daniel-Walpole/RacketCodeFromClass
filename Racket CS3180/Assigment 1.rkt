#lang racket
(require test-engine/racket-tests)
;Contract: symNumSep : L L-> (filtered list)
;Purpose: symNumSep takes a list of symbols and numbers, and filters them such
; that the symbols are first then numbers later
;Code:
(define (symNumSep L)
  (append (filter symbol? L) (filter number? L)))
;Test cases and expected outcomes
(check-expect (symNumSep  '(0 b 2 d)) '(b d 0 2))

;Contract: dupSymSpl: L->L (some items duped)
;Purpose: dupSymSpl takes a list and duplicates the symbols and deletes the strings
;Code:
(define (dupSymSpl L)
  (cond
    [(empty? L) L]
    [(string? (first L)) (append '() (dupSymSpl (rest L)))]
    [(symbol? (first L)) (append (list (first L) (first L)) (dupSymSpl (rest L)))]
    [(list? (first L)) (append (list (dupSymSpl (first L))) (dupSymSpl (rest L)))]
    [else (append (list (first L)) (dupSymSpl (rest L)))]))
;Test cases and expected outcomes
(check-expect (dupSymSpl '(a 1 b "a2" c 3)) '(a a 1 b b c c 3))

;Contract: dotProduct: l1 * l2 -> num
;Purpose: taking two list of equal lengths 
(define (dotProduct l1 l2)
  (if (eq? (length l1) (length l2))(foldr + 0 (map * l1 l2))
      "invalid input"))

(define (postfix L)
  (cond
    [(or (not (list? L)) (empty? L)) L]
    [(or (eq? '+ (car L)) (eq? '* (car L))) (append (postfix (cdr L)) (list (car L)))]
    [(list? (car L)) (cons (postfix (car L)) (postfix (cdr L)))]
    [else L]))

(define (foldr f base L)
  (cond
    [(empty? L) base]
    [(foldr f (f (last L) base) (reverse (cdr (reverse L))))]))

(define (fib num)
  (cond
    [(eq? 0 num) 0]
    [(eq? 1 num) 1]
    [(+ (fib (- num 1)) (fib (- num 2)))]))
(define (recursiveCallCountFib num)
  (cond
    [(eq? 0 num) 0]
    [(eq? 1 num) 0]
    [(eq? 2 num) 2]
    [(+ (+ (recursiveCallCountFib (- num 2)) (recursiveCallCountFib (- num 1))) 2)]))
(test)