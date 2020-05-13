#lang racket
(require test-engine/racket-tests)
;Made by Daniel Walpole
;For CS3180 Wright State University
;Contract: symNumSep : L L-> (filtered list)
;Purpose: symNumSep takes a list of symbols and numbers, and filters them such
; that the symbols are first then numbers later
;Code:
(define (symNumSep L)
  (symNumSep2 L '() '()))
(define (symNumSep2 L sL nL)
  (cond
    [(empty? L) (append nL sL)]
    [(symbol? (car L)) (symNumSep2 (cdr L) sL (append nL (list (car L))))]
    [(number? (car L)) (symNumSep2 (cdr L) (append sL (list (car L))) nL)]))
;Test cases and expected outcomes
(check-expect (symNumSep  '(0 b 2 d)) '(b d 0 2))
(check-expect (symNumSep  '(a 1 c 8 5 e d f)) '(a c e d f 1 8 5))
(check-expect (symNumSep  '()) '())

;Contract: dupSymSpl: L->L (some items duped)
;Purpose: dupSymSpl takes a list and duplicates the symbols and deletes the strings
;Code:
(define (dupSymSpl L)
  (cond
    [(empty? L) L]
    [(string? (car L)) (append '() (dupSymSpl (cdr L)))]
    [(symbol? (car L)) (append (list (car L) (car L)) (dupSymSpl (cdr L)))]
    [(list? (car L)) (append (list (dupSymSpl (car L))) (dupSymSpl (cdr L)))]
    [else (append (list (car L)) (dupSymSpl (cdr L)))]))
;Test cases and expected outcomes
(check-expect (dupSymSpl '(a 1 b "a2" c 3)) '(a a 1 b b c c 3))
(check-expect (dupSymSpl '((a 1) b (("c" c)) 2 "2")) '((a a 1) b b ((c c)) 2))
(check-expect (dupSymSpl '()) '())

;Contract: dotProduct: l1 * l2 -> num
;Purpose: taking two list of equal lengths give the dot product of those lists
;Code:
(define (dotProduct l1 l2)
  (if (eq? (length l1) (length l2))(foldr + 0 (map * l1 l2))
      "invalid input"))
;Test cases and expected outcomes
(check-expect (dotProduct '(1 2) '(3 4)) 11)
(check-expect (dotProduct '(1 2 3) '(4 5 6)) 32)
(check-expect (dotProduct '(1) '(1 2)) "invalid input")

;Contract: postfix: L -> L (postfix notation)
;Purpose: given a list postfix will output a list that is the same but in postfix notation
;Code:
(define (postfix L)
  (cond
    [(or (not (list? L)) (empty? L)) L]
    [(or (eq? '* (car L)) (eq? '+ (car L))) (append (postfix (cdr L)) (list (car L)))]
    [(list? (car L)) (cons (postfix (car L)) (postfix (cdr L)))]
    [else L]))
;Test cases and expected outcomes
(check-expect (postfix '(+ (* 1 2) (+ x y))) '((1 2 *) (x y +) +))
(check-expect (postfix '(* z 6)) '(z 6 *))
(check-expect (postfix 5) 5)
(check-expect (postfix '()) '())

;Contract: fib: num-> num
;Purpose: given a number a number will output with for its place in the fibonacci sequence
;Code:
(define (fib num)
  (cond
    [(eq? 0 num) 0]
    [(eq? 1 num) 1]
    [(+ (fib (- num 1)) (fib (- num 2)))]))
;Test cases and expected outcomes
(check-expect (fib 0) 0)
(check-expect (fib 1) 1)
(check-expect (fib 9) 34)

;Contract: recursiveCallCountFib: num-> num
;Purpose: given a spot in the fibonacci sequence output how many recursive calls have been made
;Code:
(define (recursiveCallCountFib num)
  (cond
    [(eq? 0 num) 0]
    [(eq? 1 num) 0]
    [(eq? 2 num) 2]
    [(+ (+ (recursiveCallCountFib (- num 2)) (recursiveCallCountFib (- num 1))) 2)]))
;Test cases and expected outcomes
(check-expect (map recursiveCallCountFib '(0 1 2 3 4 5 6)) '(0 0 2 4 8 14 24))
(test)