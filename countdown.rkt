;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname countdown) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; An Operator (Op) is (anyof '+ '- '* '/)

;; An Binary Expression Tree (BET) is one of:
;; * Nat
;; * (list Op BET BET)

;; (swap i j lst) produces lst with the elements at positions
;;   i and j swapped
;; swap: Nat Nat (listof X) -> (listof X)
;; requires: i, j < (length lst)
;; Example:
(check-expect (swap 0 3 '(0 1 2 3 4 5)) '(3 1 2 0 4 5))
(check-expect (swap 3 3 '(a b c d)) '(a b c d))
(check-expect (swap 4 2 '(r d 3 5 e)) '(r d e 5 3))

(define (swap i j lst)
  (local [(define (swap/counter i j counter lst-now)
            (cond [(empty? lst-now) empty]
                  [(= i counter)
                   (cons (list-ref lst j)
                         (swap/counter i j (add1 counter) (rest lst-now)))]
                  [(= j counter)
                   (cons (list-ref lst i)
                         (swap/counter i j (add1 counter) (rest lst-now)))]
                  [else
                   (cons (first lst-now)
                         (swap/counter i j (add1 counter) (rest lst-now)))]))]
    (swap/counter i j 0 lst)))

;; Tests:
(check-expect (swap 0 1 '(1 2 3)) '(2 1 3))
(check-expect (swap 1 0 '(1 2 3)) '(2 1 3))
(check-expect (swap 0 0 '(1 2 3)) '(1 2 3))
(check-expect (swap 1 2 '(2 1 1)) '(2 1 1))
(check-expect (swap 0 2 '(2 1 1)) '(1 1 2))
(check-expect (swap 0 3 '(1 2 3 1)) '(1 2 3 1))
(check-expect (swap 3 4 '(a b c c d)) '(a b c d c))
(check-expect (swap 0 4 '(a a a a c)) '(c a a a a))
(check-expect (swap 2 4 '(+ + r r 3 4)) '(+ + 3 r r 4))

;; (rod lst) consumes a list and gets rid of all the duplicates in lst
;; rod: (listof X) -> (listof X)
;; Examples:
(check-expect (rod '((1 2) (1 2) (3 2))) '((1 2) (3 2)))
(check-expect (rod empty) empty)

(define (rod lst)
  (local [(define (rod/acc lst visited)
            (cond [(empty? lst) empty]
                  [(member? (first lst) visited)
                   (rod/acc (rest lst) visited)]
                  [else
                   (cons (first lst)
                         (rod/acc (rest lst)
                                  (append visited (list (first lst)))))]))]
    (rod/acc lst empty)))

;; (generate-permutations lst) produces all possible permutations of lst
;; generate-permutations: (listof X) -> (listof (listof X))
;; Examples:
(check-expect
 (generate-permutations '(2 4 8))
 '((2 4 8) (4 2 8) (8 4 2) (2 8 4) (4 8 2) (8 2 4)))
(check-expect
 (generate-permutations '(1 2 3))
 '((1 2 3) (2 1 3) (3 2 1) (1 3 2) (2 3 1) (3 1 2)))
(check-expect (generate-permutations empty) empty)
(check-expect (generate-permutations '(1 2)) '((1 2) (2 1)))

(define (generate-permutations lst)
  (local
    [(define (first-swap lst parent)
       (local [(define len (length lst))
               (define len-lst (build-list len (lambda (x) x)))
               (define new-lst
                 (foldr (lambda (x y) (cons (append parent (swap 0 x lst)) y))
                        empty len-lst))
               (define swap-lst (foldr (lambda (x y) (cons (swap 0 x lst) y))
                                       empty len-lst))]
         (append new-lst (swap-all swap-lst parent))))
     (define (swap-all lst parent)
       (cond [(empty? lst) empty]
             [else
              (local [(define parent-now
                        (append parent (list (first (first lst)))))
                      (define children (rest (first lst)))]
                (append (first-swap children parent-now)
                        (swap-all (rest lst) parent)))]))]
    (rod (first-swap lst empty))))
;; Tests:
(check-expect (generate-permutations '(5 6)) '((5 6) (6 5)))
(check-expect (generate-permutations '(5 5)) '((5 5)))
(check-expect (generate-permutations '(6 6 7)) '((6 6 7) (7 6 6) (6 7 6)))
(check-expect (generate-permutations '(1)) '((1)))
(check-expect
 (generate-permutations '(3 4 5))
 '((3 4 5) (4 3 5) (5 4 3) (3 5 4) (4 5 3) (5 3 4)))
(check-expect (generate-permutations '(8 2)) '((8 2) (2 8)))
(check-expect (generate-permutations '(8 8 8)) '((8 8 8)))

;; (map-append lst v) consumes a list and v and create a list that contains v
;;   with every element in lst
;; map-append: (listof any) any -> (listof any)
;; Examples:
(check-expect (map-append '(1 2 3 4) 'x) '((x 1) (x 2) (x 3) (x 4)))
(check-expect (map-append empty 'x) empty)

(define (map-append lst v)
  (map (lambda (x) (cond [(list? x) (append (list v) x)]
                         [else (cons v (list x))])) lst))

;; (generate-tuples lst n) produces all tuples of length n of elements in lst
;; generate-tuples: (listof X) Nat -> (listof (listof X))
;; Examples:
(check-expect
 (generate-tuples '(+ -) 3)
 '((+ + +) (+ + -) (+ - +) (+ - -) (- + +) (- + -) (- - +) (- - -)))
(check-expect (generate-tuples '(+ -) 0) (list empty))

(define (generate-tuples lst n)
  (local [(define (map-append-lst lst1 lst2)
            (foldr (lambda (x y) (append (map-append lst1 x) y)) empty lst2))
          (define (generate-tuples/wd lst n)
            (cond [(zero? n) '(())]
                  [(= 1 n) (map (lambda (x) (list x)) lst)]
                  [else (foldr (lambda (x y) (map-append-lst y lst))
                               lst
                               (build-list (sub1 n) (lambda (x) x)))]))]
    (rod (generate-tuples/wd lst n))))

    
(check-expect (generate-tuples '(+) 1) '((+)))
(check-expect (generate-tuples '(+ *) 2) '((+ +) (+ *) (* +) (* *)))
(check-expect (generate-tuples '(+ - * /) 1) '((+) (-) (*) (/)))
(check-expect (generate-tuples '(* /) 1) '((*) (/)))
(check-expect (generate-tuples '(* + -) 2)
              '((* *) (* +) (* -) (+ *) (+ +) (+ -) (- *) (- +) (- -)))
(check-expect (generate-tuples '(* * *) 3) '((* * *)))
(check-expect (generate-tuples '(* * *) 1) '((*)))
(check-expect
 (generate-tuples '(* -) 4)
 '((* * * *) (* * * -) (* * - *) (* * - -) (* - * *)
             (* - * -) (* - - *) (* - - -) (- * * *) (- * * -) (- * - *)
             (- * - -) (- - * *) (- - * -) (- - - *) (- - - -)))
(check-expect (generate-tuples '(* -) 3)
              '((* * *) (* * -) (* - *) (* - -) (- * *) (- * -)
                        (- - *) (- - -)))
(check-expect (generate-tuples '(+) 3) '((+ + +)))

;; (create-left-right-pairs nodes-cnt) consumes a overall node-count on a tree
;;   and produces a list of number-pairs that indicate the size of all possible
;;   left and right subtrees
;; create-left-right-pairs: Nat -> (listof (list Nat Nat))
;; Examples:
(check-expect (create-left-right-pairs 3) '((2 0) (1 1) (0 2)))
(check-expect (create-left-right-pairs 0) empty)
(check-expect (create-left-right-pairs 5) '((4 0) (3 1) (2 2) (1 3) (0 4)))

(define (create-left-right-pairs nodes-cnt)
  (local [(define node-lst (build-list nodes-cnt (lambda (x) x)))
          (define node-lst-rv (reverse node-lst))]
    (map (lambda (left right) (list left right)) node-lst-rv node-lst)))

;; Tests:
(check-expect (create-left-right-pairs 1) '((0 0)))
(check-expect (create-left-right-pairs 2) '((1 0) (0 1)))
(check-expect (create-left-right-pairs 4) '((3 0) (2 1) (1 2) (0 3)))

;; (create-tree-structure node-cnt) consumes an overall node-count and produces
;;   a list of trees with the internal nodes replaced by empty
;; create-tree-structure: Nat -> (nested (listof Nat))
;; Examples:
(check-expect (create-tree-structure 3)
              '((3 (2 (1 empty empty) empty) empty)
                (3 (2 empty (1 empty empty)) empty)
                (3 (1 empty empty) (1 empty empty))
                (3 empty (2 (1 empty empty) empty))
                (3 empty (2 empty (1 empty empty)))))
(check-expect (create-tree-structure 2)
              '((2 (1 empty empty) empty)
                (2 empty (1 empty empty))))
(check-expect (create-tree-structure 1) '((1 empty empty)))
(check-expect (create-tree-structure 0) '(empty))

(define (create-tree-structure nodes-cnt)
  (local [(define (create-tree-pair-lst pair-lst node)
            (cond [(empty? pair-lst) empty]
                  [else
                   (append
                    (map-append (create-tree-pair (first pair-lst) node) node)
                    (create-tree-pair-lst (rest pair-lst) node))]))
          (define (create-tree-pair pair node)
            (local [(define left (first pair))
                    (define right (second pair))
                    (define left-tree (create-tree-structure left))
                    (define right-tree (create-tree-structure right))]
              (foldr
               (lambda (l next)
                 (append (map (lambda (r) (list l r)) right-tree) next))
               empty left-tree)))]
    (cond [(zero? nodes-cnt) '(empty)]
          [else (create-tree-pair-lst
                 (create-left-right-pairs nodes-cnt) nodes-cnt)])))
;; Tests:
(check-expect (length (create-tree-structure 3)) 5)
(check-expect (length (create-tree-structure 4)) 14)
(check-expect (length (create-tree-structure 5)) 42)
(check-expect (length (create-tree-structure 6)) 132)
(check-expect (length (create-tree-structure 7)) 429)

;; (create-bets nlon nloop) produces a list of all possible BET based off
;;   of nlon and nloop, where nlon is a list of permutations of a list of
;;   numbers and nloop is a list of n-tuples of a list of operators
;; create-bets: (listof (listof Num)) (listof (listof Op)) -> (listof BET)
;; requires: (length nlon) - (length nloop) = 1
;; Examples:
(check-expect
 (create-bets
  '((8 6 4 2))
  '((/ + -)))
 '((/ (+ (- 8 6) 4) 2)
   (/ (+ 8 (- 6 4)) 2)
   (/ (+ 8 6) (- 4 2))
   (/ 8 (+ (- 6 4) 2))
   (/ 8 (+ 6 (- 4 2)))))
(check-expect (create-bets empty empty) empty)

(define (create-bets nlon nloop)
  (local [(define (get-left-op bet-st lst)
            (cond [(empty? lst) empty]
                  [(list? bet-st)
                   (build-list (first bet-st) (lambda (x) (list-ref lst x)))]
                  [else (list (first lst))]))
          (define (get-right-op bet-st lst)
            (cond [(empty? lst) empty]
                  [(list? bet-st)
                   (reverse
                    (build-list (first bet-st)
                                (lambda (x) (list-ref (reverse lst) x))))]
                  [else (list (first (reverse lst)))]))
          (define (get-left-n bet-st lst)
            (cond ;[(empty? lst) empty]
              [(list? bet-st)
               (build-list (add1 (first bet-st))
                           (lambda (x) (list-ref lst x)))]
              [else (list (first lst))]))
          (define (get-right-n bet-st lst)
            (cond ;[(empty? lst) empty]
              [(list? bet-st)
               (reverse
                (build-list (add1 (first bet-st))
                            (lambda (x) (list-ref (reverse lst) x))))]
              [else (list (first (reverse lst)))]))
          (define (match-bet bet-st op-lst n-lst)
            (cond [(symbol? bet-st) (first n-lst)] 
                  [else
                   (local [(define left-tree (second bet-st))
                           (define right-tree (third bet-st))
                           (define rest-op (rest op-lst))
                           (define op-now (first op-lst))]
                     (list
                      op-now 
                      (match-bet left-tree (get-left-op left-tree rest-op)
                                 (get-left-n left-tree n-lst))
                      (match-bet right-tree (get-right-op right-tree rest-op)
                                 (get-right-n right-tree n-lst))))]))
          (define (flatten-my-list lst)
            (cond [(empty? lst) empty]
                  [(symbol? (first (first lst)))
                   (cons (first lst) (flatten-my-list (rest lst)))]
                  [else (flatten-my-list (append (first lst) (rest lst)))]))]
    (cond [(or (empty? nlon) (empty? nloop)) empty]
          [else
           (flatten-my-list
            (map (lambda (lon)
                   (map (lambda (loop)
                          (map (lambda (lot) (match-bet lot loop lon))
                               (create-tree-structure
                                (length (first nloop))))) nloop)) nlon))])))
;; Tests:
(check-expect (create-bets '((3 2) (2 3)) '((-) (+)))
              '((- 3 2) (+ 3 2) (- 2 3) (+ 2 3)))
(check-expect
 (length (create-bets '((2 8 4) (4 8 2) (8 4 2)) '((- -) (+ /) (+ +) (* *))))
 (* 3 4 2))
(check-expect
 (create-bets '((8 6 7) (6 7 8)) '((+ -) (- +)))
 '((+ (- 8 6) 7)
   (+ 8 (- 6 7))
   (- (+ 8 6) 7)
   (- 8 (+ 6 7))
   (+ (- 6 7) 8)
   (+ 6 (- 7 8))
   (- (+ 6 7) 8)
   (- 6 (+ 7 8))))
(check-expect
 (create-bets '((1 2 3 4)) '((+ - * /)))
 (list
  (list '+ (list '- (list '* (list '/ 1 2) 3) 4) 4)
  (list '+ (list '- (list '* 1 (list '/ 2 3)) 4) 4)
  (list '+ (list '- (list '* 1 2) (list '/ 3 4)) 4)
  (list '+ (list '- 1 (list '* (list '/ 2 3) 4)) 4)
  (list '+ (list '- 1 (list '* 2 (list '/ 3 4))) 4)
  (list '+ (list '- (list '* 1 2) 3) (list '/ 3 4))
  (list '+ (list '- 1 (list '* 2 3)) (list '/ 3 4))
  (list '+ (list '- 1 2) (list '* (list '/ 2 3) 4))
  (list '+ (list '- 1 2) (list '* 2 (list '/ 3 4)))
  (list '+ 1 (list '- (list '* (list '/ 1 2) 3) 4))
  (list '+ 1 (list '- (list '* 1 (list '/ 2 3)) 4))
  (list '+ 1 (list '- (list '* 1 2) (list '/ 3 4)))
  (list '+ 1 (list '- 1 (list '* (list '/ 2 3) 4)))
  (list '+ 1 (list '- 1 (list '* 2 (list '/ 3 4))))))
              

;; (evaluate-bets lobet target) produces a list of all BET from lobet
;;   that evaluates to the target value
;; evaluate-bets: (listof BET) Nat -> (listof BET)
;; Example:
(check-expect (evaluate-bets (create-bets (generate-permutations '(2 4 8))
                                          (generate-tuples '(+ - *) 2)) 2)
              '((- 8 (+ 4 2))
                (- (- 8 4) 2)
                (- 8 (+ 2 4))
                (- (- 8 2) 4)))
(check-expect (evaluate-bets (create-bets '((3 2) (2 3)) '((-) (+))) 5)
              '((+ 3 2) (+ 2 3)))
(check-expect (evaluate-bets (create-bets '((8 6 7) (6 7 8)) '((+ -) (- +))) 7)
              '((- (+ 8 6) 7)))
(check-expect (evaluate-bets (create-bets '((8 6 7) (6 7 8)) '((+ -) (- +))) 5)
              '((- (+ 6 7) 8)))
(check-expect
 (evaluate-bets (create-bets '((8 6 7) (6 7 8)) '((+ -) (- +))) 0) empty)
(check-expect (evaluate-bets (create-bets '((8 6 6)) '((/ -))) 0) empty)
(check-expect (evaluate-bets (create-bets '((4 2 3 3)) '((+ / -))) 4) empty)
(check-expect (evaluate-bets (create-bets '((4 5 3)) '((+ /))) 5) empty)

(define (evaluate-bets lobet target)
  (local [(define (find-op op-sym)
            (cond [(symbol=? op-sym '+) +]
                  [(symbol=? op-sym '-) -]
                  [(symbol=? op-sym '*) *]
                  [else /]))
          (define (nat? n)
            (and (integer? n) (>= n 0)))
          (define (not-division-by-zero? op right)
            (cond [(equal? op /) (and (nat? right) (not (zero? right)))]
                  [else true]))
          (define (evaluate bet-info)
            (local [(define left (second bet-info))
                    (define right (third bet-info))
                    (define op (find-op (first bet-info)))]
              (cond [(and (list? left)
                          (list? right)
                          (not-division-by-zero? op (evaluate right))
                          (nat? (evaluate left))
                          (nat? (evaluate right)))
                     (op (evaluate left) (evaluate right))]
                    [(and (nat? right)
                          (list? left)
                          (nat? (evaluate left))
                          (not-division-by-zero? op right)
                          (nat? (op (evaluate left) right)))
                     (op (evaluate left) right)]
                    [(and (nat? left)
                          (list? right)
                          (nat? (evaluate right))
                          (not-division-by-zero? op (evaluate right))
                          (nat? (op left (evaluate right))))
                     (op left (evaluate right))]
                    [(and (nat? left)
                          (nat? right)
                          (not-division-by-zero? op right)
                          (nat? (op left right)))
                     (op left right)]
                    [else false])))
          (define valid?
            (lambda (bet) (and (not (false? (evaluate bet)))
                               (= target (evaluate bet)))))]
    (filter valid? lobet)))

;; (countdown-numbers lon target) produces a BET using the numbers in lon
;;   that evaluates to target, or false if no such BET exists
;; countdown-numbers: (listof Nat) Nat -> (anyof BET false)

(define (countdown-numbers lon target)
  (local [(define solutions
            (evaluate-bets
             (create-bets (generate-permutations lon)
                          (generate-tuples '(+ - * /) 4)) target))]
    (cond [(empty? solutions) false]
          [else (first solutions)])))

(check-expect (countdown-numbers '(1 2 3 4 5) 120)
              (list '* (list '+ (list '+ 1 2) 3) (list '* 4 5)))
(check-expect (countdown-numbers '(17 28 39 40 10) 45)
              (list '+ 40 (list '/ (list '+ 28 (list '- 39 17)) 10)))
(check-expect (countdown-numbers '(1 2 3 4 5) 100)
              (list '* (list '+ (list '* 1 2) 3) (list '* 4 5)))
(check-expect (countdown-numbers '(1 1 1 1 1) 100) false)
                                 
              
















                   
          
          


         
    
  

 


