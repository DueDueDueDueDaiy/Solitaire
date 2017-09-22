#lang racket
;; The following line is REQUIRED (do not remove)
(require "lib.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A Dimension is an Int
;; requires: 1 <= Dimension <= 9

;; A Peg [position] is an Int
;; requires: 11 <= Peg <= 99
;;           neither digit can be zero or greater than the
;;             Dimension (for the corresponding board)

;; A Board is a (list Dimension (listof Peg))
;; The list contains INVALID Peg positions

;; A State is a (listof Peg)
;; requires: list is non-emtpy
;;           each Peg is VALID for the corresponding board

;; A Solution is one of:
;; * 'any
;; * Peg


(define no-solution-text (list (list "No Solution Found")))


(define sample (list 4 (list 41 42 43 44)))
#|
....
....
....
    
|#

(define sample/init (list 22 23))
#|
....
.OO.
....
    
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this is the traditional cross pattern with default init state cross/init
;; with some additional (easier) init states you can use

(define cross (list 7 (list 11 12 16 17 21 22 26 27 61 62 66 67 71 72 76 77)))
#|
  ...  
  ...  
.......
.......
.......
  ...  
  ...  
|#

(define cross/init (list 13 14 15 23 24 25 31 32 33 34 35 36 37 41 42 43
                         45 46 47 51 52 53 54 55 56 57 63 64 65 73 74 75))
#|
  OOO  
  OOO  
OOOOOOO
OOO.OOO
OOOOOOO
  OOO  
  OOO  
|#

(define cross/submarine (list 34 42 43 44 45 46))
#|
  ...  
  ...  
...O...
.OOOOO.
.......
  ...  
  ...  
|#

(define cross/greek (list 24 34 42 43 44 45 46 54 64))
#|
  ...  
  .O.  
...O...
.OOOOO.
...O...
  .O.  
  ...  
|#

(define cross/small-diamond (list 24 33 34 35 42 43 45 46 53 54 55 64))
#|
  ...  
  .O.  
..OOO..
.OO.OO.
..OOO..
  .O.  
  ...  
|#

(define cross/big-diamond (list 14 23 24 25 32 33 34 35 36 41 42 43
                                45 46 47 52 53 54 55 56 63 64 65 74))
#|
  .O.  
  OOO  
.OOOOO.
OOO.OOO
.OOOOO.
  OOO  
  .O.  
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;(a)
;;purpose: function build-board consumes a Dimension 
;;         and produces a (listof (listof Peg)) corresponding to a list of rows, 
;;         with each row containing a list of Peg positions in that row.
;;contract: Nat -> (listof (listof Peg))
;;examples:
(check-expect (build-board 3) '((11 12 13) (21 22 23) (31 32 33)))
(check-expect (build-board 2) '((11 12) (21 22)))

(define (build-board n)
  (build-list n (lambda (x) (map (lambda (y) (+ (* 10 (add1 x)) y)) 
                                 (build-list n (lambda (x) (add1 x)))))))

;;tests:
(check-expect (build-board 3) '((11 12 13) (21 22 23) (31 32 33)))
(check-expect (build-board 2) '((11 12) (21 22)))
(check-expect (build-board 4) '((11 12 13 14) (21 22 23 24) (31 32 33 34) (41 42 43 44)))
(check-expect (build-board 1) '((11)))



;;(b)
;;purpose: function state->los consumes (in order) a Board and a State 
;;         and produces a (listof Str) corresponding to one string per row.
;;contract: Board State -> (Listof Str)
;;examples:
(check-expect (state->los '(4 (41 42 43 44)) '(22 23))
              '("...." ".OO." "...." "    "))
(check-expect (state->los '(4 (43 44)) '(22 23))
              '("...." ".OO." "...." "..  "))

(define (state->los boa sta)
  (local [;;purpose: function change-form1 consumes a list of num
          ;;         and produces a list to Pair (list Num Str)
          ;;contract: (Listof Nat) -> (Listof Pair)
          (define (change-form1 lon)
            (cond [(empty? lon) lon]
                  [(cons? (first lon)) 
                   (cons (change-form1 (first lon)) 
                         (change-form1 (rest lon)))]
                  [(number? (first lon)) 
                   (cons (list (first lon) #\.) 
                         (change-form1 (rest lon)))]))
          ;;purpose: function change-form2 consumes a list of num
          ;;         and produces a list to Pair (list Num Str)
          ;;contract: (Listof Nat) -> (Listof Pair)
          (define (change-form2 lon)
            (cond [(empty? lon) lon]
                  [(number? (first lon)) 
                   (cons (list (first lon) #\space) 
                         (change-form2 (rest lon)))]))
          ;;purpose: function change-form3 consumes a list of num
          ;;         and produces a list to Pair (list Num Str)
          ;;contract: (Listof Nat) -> (Listof Pair)
          (define (change-form3 lon)
            (cond [(empty? lon) lon]
                  [(number? (first lon)) 
                   (cons (list (first lon) #\O) 
                         (change-form3 (rest lon)))]))
          ;;purpose: function change1 change2 change3 work together
          ;;         consuming a list of dimension and peg and a list of state
          ;;         and producing a list of pairs
          ;;contract: Board State -> (Listof Pair)
          (define (change1 lst1 lst2)
            (cond [(empty? lst2) lst2]
                  [(= (first (first lst2)) (first lst1)) 
                   (cons lst1 (change1 lst1 (rest lst2)))]
                  [else (cons (first lst2) 
                              (change1 lst1 (rest lst2)))]))
          (define (change2 lst1 lst2)
            (cond [(empty? lst2) lst2]
                  [else (cons (change1 lst1 (first lst2)) 
                              (change2 lst1 (rest lst2)))]))
          (define (change3 lst1 lst2)
            (cond [(empty? lst1) lst2]
                  [else (change3 (rest lst1) 
                                 (change2 (first lst1) lst2))]))
          ;;purpose: function simp consumes a list of pair
          ;;         and produecs a list of str
          ;;contract: (Listof Pair) -> (Listof Str)
          (define (simp lst)
            (cond [(empty? lst) empty]
                  [else (cons (second (first lst)) 
                              (simp (rest lst)))]))]
    (map list->string 
         (map simp 
              (cond [(empty? sta) "error: a State is non-empty list"]
                    [else (change3 
                           (change-form3 sta)
                           (cond [(empty? (second boa)) 
                                  (change-form1 (build-board (first boa)))] 
                                 [else (change3 
                                        (change-form2 (second boa))
                                        (change-form1 (build-board (first boa))))]))])))))

;;tests:
(check-expect (state->los '(4 (41 42 43 44)) '(22 23))
              '("...." ".OO." "...." "    "))
(check-expect (state->los '(4 (43 44)) '(22 23))
              '("...." ".OO." "...." "..  "))
(check-expect (state->los '(3 (13 23 33)) '(11 21 31))
              '("O. " "O. " "O. "))
(check-expect (state->los '(5 (11 15 51 55)) '(23 32 33 34 43))
              '(" ... " "..O.." ".OOO." "..O.." " ... "))
(check-expect (state->los '(6 ()) '(21 22 24 26 31 33 35 41 43 45 51 52 55))
              '("......" "OO.O.O" "O.O.O." "O.O.O." "OO..O." "......"))



;;(c)
;;purpose: function generator make-solved? that consumes a Solution 
;;         and generates a new predicate function that consumes a State 
;;         and produces true if the state is a solution, and false otherwise.
;;contract: Solution -> (State -> Boll)
;;examples:
(check-expect (my-solved? (list 45)) true)
(check-expect (my-solved? (list 44 45)) false)

(define (make-solved? soln) 
  (lambda (sta) (cond [(and (equal? soln 'any) (empty? (rest sta))) true]
                      [(and (number? soln) (equal? (first sta) soln) (empty? (rest sta))) true]
                      [else false])))

(define my-solved? (make-solved? 'any))

;;tests:
(check-expect (my-solved? (list 32)) true)
(check-expect (my-solved? (list 32 33 34)) false)



;;(d)
;;purpose: function neighbours consumes (in order) a Board and a State 
;;         and produces a (listof State) which corresponds to every possible legal move from the consumed state.
;;contract: Board State -> (Listof State)
;;examples:
(check-expect (neighbours (list 4 (list 41 42 43 44)) (list 22 23))
              (list (list 24) (list 21)))
(check-expect (neighbours (list 7 empty) '(24 25 34 35))
              '((25 35 44) (26 34 35) (24 34 45) (23 34 35) (14 25 35) (24 25 36) (15 24 34) (24 25 33)))

(define (neighbours boa sta)
  (local [;;purpose: function remove-lst consumes list1 and list2
          ;;         and produces a new list2 
          ;;         by removing all the elements of list1 from original list2
          ;;contract: (Listof Any) (Listof Any) -> (Listof Any)
          (define (remove-lst lst1 lst2)
            (cond [(empty? lst1) lst2]
                  [else (remove-lst (rest lst1) (remove (first lst1) lst2))]))
          ;;purpose: function valid-peg consumes a Borad 
          ;;         and produces a list of all the valid pegs
          ;;contract: Board -> (Listof Peg)
          (define (valid-peg boa)
            (cond [(empty? (second boa)) 
                   (foldr append empty (build-board (first boa)))]
                  [else 
                   (foldr append empty 
                          (map (lambda (x) 
                                 (cond [(empty? (second boa)) empty]
                                       [else (remove-lst (second boa) x)]))
                               (build-board (first boa))))]))
          ;;purpose: function up down left right consumes a Board, a State and a Peg
          ;;         and produces a new State after one chess being moved up down left right
          ;;contract: Board State Nat -> State
          (define (up boa sta n)
            (cond [(empty? (rest sta)) empty]
                  [(and (member? (- n 20) (valid-peg boa))
                        (member? (- n 10) (remove n sta))
                        (not (member? (- n 20) (remove n sta))))
                   (sort (cons (- n 20) (remove (- n 10) (remove n sta))) <)]
                  [else empty]))
          (define (down boa sta n)
            (cond [(empty? (rest sta)) empty]
                  [(and (member? (+ n 20) (valid-peg boa))
                        (member? (+ n 10) (remove n sta))
                        (not (member? (+ n 20) (remove n sta))))
                   (sort (cons (+ n 20) (remove (+ n 10) (remove n sta))) <)]
                  [else empty]))
          (define (left boa sta n)
            (cond [(empty? (rest sta)) empty]
                  [(and (member? (- n 2) (valid-peg boa))
                        (member? (- n 1) (remove n sta))
                        (not (member? (- n 2) (remove n sta))))
                   (sort (cons (- n 2) (remove (- n 1) (remove n sta))) <)]
                  [else empty]))
          (define (right boa sta n)
            (cond [(empty? (rest sta)) empty]
                  [(and (member? (+ n 2) (valid-peg boa))
                        (member? (+ n 1) (remove n sta))
                        (not (member? (+ n 2) (remove n sta))))
                   (sort (cons (+ n 2) (remove (+ n 1) (remove n sta))) <)]
                  [else empty]))
          ;;purpose: function remove-empty consumes a list of list
          ;;         and produces a new list 
          ;;         by removing all the empty list from the original list
          (define (remove-empty lst)
            (cond [(empty? lst) empty]
                  [(number? (first lst)) (list lst)]
                  [else (append (remove-empty (first lst)) 
                                (remove-empty (rest lst)))]))]
    (cond [(empty? (rest sta)) (list sta)]
          [else (remove-empty 
                 (map (lambda (x) (list (up boa sta x) 
                                        (down boa sta x) 
                                        (left boa sta x) 
                                        (right boa sta x))) 
                      sta))])))

;;tests:
(check-expect (neighbours (list 4 (list 41 42 43 44)) (list 22))
              (list (list 22)))
(check-expect (neighbours (list 5 empty) '(24 25 34 35))
              '((25 35 44) (24 34 45) (23 34 35) (14 25 35) (15 24 34) (24 25 33)))
(check-expect (neighbours (list 5 (list 41 42 43 44 45 51 52 53 54 55)) '(24 25 34 35))
              '((23 34 35) (14 25 35) (15 24 34) (24 25 33)))



(define (make-neighbours board) (lambda (state) (neighbours board state)))



;;(e)
;;purpose: function solitaire consumes (in order) a Board, an initial State, and a Solution. 
;;         If a solution exists, solitaire produces a (listof State) 
;;         that corresponds to a sequence of states starting with the the initial state 
;;         and ending with the solution state. 
;;         solitaire produces false if no solution exists.
;;contract: Board State Solution -> (Anyof (Listof State) Bool)
;;examples:
(check-expect (solitaire '(3 ()) '(11 12) 'any)
              (list '(11 12) '(13)))
(check-expect (solitaire (list 4 (list 41 42 43 44)) '(22 23) 'any)
              (list '(22 23) '(24)))

(define (solitaire boa sta soln)
  (find-route sta (make-neighbours boa) my-solved?))

;;tests:
(check-expect (solitaire '(4 ()) '(11 44) 'any) false)
(check-expect (solitaire (list 7 empty) '(24 25 34 35) 'any) 
              (list (list 24 25 34 35) (list 25 35 44) (list 44 45) (list 46)))
(check-expect (solitaire cross cross/greek 'any) 
              (list
               (list 24 34 42 43 44 45 46 54 64)
               (list 14 42 43 44 45 46 54 64)
               (list 14 34 42 43 45 46 64)
               (list 14 34 44 45 46 64)
               (list 14 24 45 46 64)
               (list 34 45 46 64)
               (list 34 44 64)
               (list 54 64)
               (list 74)))
(check-expect (solitaire cross cross/submarine 'any)
              (list
               (list 34 42 43 44 45 46)
               (list 42 43 45 46 54)
               (list 44 45 46 54)
               (list 34 45 46)
               (list 34 44)
               (list 54)))
(check-expect (solitaire cross cross/small-diamond 'any)
              (list
               (list 24 33 34 35 42 43 45 46 53 54 55 64)
               (list 33 35 42 43 44 45 46 53 54 55 64)
               (list 23 35 42 44 45 46 53 54 55 64)
               (list 23 35 42 44 46 53 54 64 65)
               (list 23 34 35 42 46 53 64 65)
               (list 23 33 42 46 53 64 65)
               (list 42 43 46 53 64 65)
               (list 44 46 53 64 65)
               (list 44 46 53 63)
               (list 43 44 46)
               (list 45 46)
               (list 47)))



;;(f)
;;purpose: function result->text that consumes (in order) a Board and a result from solitaire 
;;         and produces a (listof (listof Str)) where each (listof Str) is the value 
;;         produced by state->los for the corresponding state in the sequence. 
;;         If no solution is found, this function produces the provided constant no-solution-text.
;;contract: Board (Anyof (Listof State) Bool) -> (listof (listof Str)
;;examples:
(check-expect (result->text '(3 ()) (list '(11 12) '(13)))
              (list '("OO."
                      "..."
                      "...")
                    '("..O"
                      "..."
                      "...")))
(check-expect (result->text cross (solitaire cross cross/submarine 'any))
              (list
               (list
                "  ...  "
                "  ...  "
                "...O..."
                ".OOOOO."
                "......."
                "  ...  "
                "  ...  ")
               (list
                "  ...  "
                "  ...  "
                "......."
                ".OO.OO."
                "...O..."
                "  ...  "
                "  ...  ")
               (list
                "  ...  "
                "  ...  "
                "......."
                "...OOO."
                "...O..."
                "  ...  "
                "  ...  ")
               (list
                "  ...  "
                "  ...  "
                "...O..."
                "....OO."
                "......."
                "  ...  "
                "  ...  ")
               (list
                "  ...  "
                "  ...  "
                "...O..."
                "...O..."
                "......."
                "  ...  "
                "  ...  ")
               (list
                "  ...  "
                "  ...  "
                "......."
                "......."
                "...O..."
                "  ...  "
                "  ...  ")))

(define (result->text boa result)
  (map (lambda (x) (state->los boa x)) result))

;;tests:
(check-expect (result->text cross (solitaire cross cross/greek 'any))
              (list
               (list
                "  ...  "
                "  .O.  "
                "...O..."
                ".OOOOO."
                "...O..."
                "  .O.  "
                "  ...  ")
               (list
                "  .O.  "
                "  ...  "
                "......."
                ".OOOOO."
                "...O..."
                "  .O.  "
                "  ...  ")
               (list
                "  .O.  "
                "  ...  "
                "...O..."
                ".OO.OO."
                "......."
                "  .O.  "
                "  ...  ")
               (list
                "  .O.  "
                "  ...  "
                "...O..."
                "...OOO."
                "......."
                "  .O.  "
                "  ...  ")
               (list
                "  .O.  "
                "  .O.  "
                "......."
                "....OO."
                "......."
                "  .O.  "
                "  ...  ")
               (list
                "  ...  "
                "  ...  "
                "...O..."
                "....OO."
                "......."
                "  .O.  "
                "  ...  ")
               (list
                "  ...  "
                "  ...  "
                "...O..."
                "...O..."
                "......."
                "  .O.  "
                "  ...  ")
               (list
                "  ...  "
                "  ...  "
                "......."
                "......."
                "...O..."
                "  .O.  "
                "  ...  ")
               (list
                "  ...  "
                "  ...  "
                "......."
                "......."
                "......."
                "  ...  "
                "  .O.  ")))
(check-expect (result->text cross (solitaire cross cross/big-diamond 'any))
              (list
               (list
                "  .O.  "
                "  OOO  "
                ".OOOOO."
                "OOO.OOO"
                ".OOOOO."
                "  OOO  "
                "  .O.  ")
               (list
                "  .O.  "
                "  O.O  "
                ".OO.OO."
                "OOOOOOO"
                ".OOOOO."
                "  OOO  "
                "  .O.  ")
               (list
                "  .O.  "
                "  O.O  "
                "...OOO."
                "OOOOOOO"
                ".OOOOO."
                "  OOO  "
                "  .O.  ")
               (list
                "  .OO  "
                "  O..  "
                "...O.O."
                "OOOOOOO"
                ".OOOOO."
                "  OOO  "
                "  .O.  ")
               (list
                "  O..  "
                "  O..  "
                "...O.O."
                "OOOOOOO"
                ".OOOOO."
                "  OOO  "
                "  .O.  ")
               (list
                "  ...  "
                "  ...  "
                "..OO.O."
                "OOOOOOO"
                ".OOOOO."
                "  OOO  "
                "  .O.  ")
               (list
                "  ...  "
                "  ...  "
                "....OO."
                "OOOOOOO"
                ".OOOOO."
                "  OOO  "
                "  .O.  ")
               (list
                "  ...  "
                "  ...  "
                "......O"
                "OOOOOOO"
                ".OOOOO."
                "  OOO  "
                "  .O.  ")
               (list
                "  ...  "
                "  ...  "
                "......."
                "OOOOOO."
                ".OOOOOO"
                "  OOO  "
                "  .O.  ")
               (list
                "  ...  "
                "  ...  "
                "......."
                "OOOO..O"
                ".OOOOOO"
                "  OOO  "
                "  .O.  ")
               (list
                "  ...  "
                "  ...  "
                "..O...."
                "OO.O..O"
                ".O.OOOO"
                "  OOO  "
                "  .O.  ")
               (list
                "  ...  "
                "  ...  "
                "..O...."
                "..OO..O"
                ".O.OOOO"
                "  OOO  "
                "  .O.  ")
               (list
                "  ...  "
                "  ...  "
                "......."
                "...O..O"
                ".OOOOOO"
                "  OOO  "
                "  .O.  ")
               (list
                "  ...  "
                "  ...  "
                "......."
                "...O..O"
                ".O.OOOO"
                "  .OO  "
                "  OO.  ")
               (list
                "  ...  "
                "  ...  "
                "......."
                "...OO.O"
                ".O.O.OO"
                "  .O.  "
                "  OO.  ")
               (list
                "  ...  "
                "  ...  "
                "......."
                ".....OO"
                ".O.O.OO"
                "  .O.  "
                "  OO.  ")
               (list
                "  ...  "
                "  ...  "
                "......."
                "....O.."
                ".O.O.OO"
                "  .O.  "
                "  OO.  ")
               (list
                "  ...  "
                "  ...  "
                "......."
                "....O.."
                ".O.OO.."
                "  .O.  "
                "  OO.  ")
               (list
                "  ...  "
                "  ...  "
                "......."
                "......."
                ".O.O..."
                "  .OO  "
                "  OO.  ")
               (list
                "  ...  "
                "  ...  "
                "......."
                "......."
                ".O.O..."
                "  .OO  "
                "  ..O  ")
               (list
                "  ...  "
                "  ...  "
                "......."
                "......."
                ".O.OO.."
                "  .O.  "
                "  ...  ")
               (list
                "  ...  "
                "  ...  "
                "......."
                "......."
                ".OO...."
                "  .O.  "
                "  ...  ")
               (list
                "  ...  "
                "  ...  "
                "......."
                "......."
                "...O..."
                "  .O.  "
                "  ...  ")
               (list
                "  ...  "
                "  ...  "
                "......."
                "......."
                "......."
                "  ...  "
                "  .O.  ")))

; (show (result->text cross (solitaire cross cross/init 'any)))
