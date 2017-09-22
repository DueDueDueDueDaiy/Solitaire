#lang racket

(provide find-route find-final disp show lists-equiv?)
;; find-route: useful for "solitaire" (and debugging your "sudoku")
;; find-final: useful for "sudoku"
;; disp: useful for debugging your code
;; show: useful for visualizing "solitaire" (and debugging your "sudoku")
;; lists-equiv?: useful for testing

(require slideshow)

;; Note that in the following contracts, X is your State

;; (find-route initial-state neighbours solved?) produces a sequence of
;;   states from initial-state to a solution, or false if no solution exists.
;;   solved? determines if a state is a solution.
;;   neighbours produces a list of legal next states from a given state.
;; find-route: X (X -> (listof X)) (X -> Bool) -> (anyof false (listof X))
(define (find-route initial-state neighbours solved?)
  (local
    [;; a NoRoute is a (make-noroute (listof Any))
     ;; a special structure to store unsuccessful visited states
     (define-struct noroute (visited))
     
     ;; (find-route/acc state visited) searches outward from state 
     ;;   looking for a path to a solution, accumulating visited states
     ;; find-route: X (listof X) -> (anyof NoRoute (listof X))
     (define (find-route/acc state visited)
       (cond [(solved? state) (list state)]
             [else 
              (local [(define route (find-route/list (neighbours state)
                                                     (cons state visited)))]
                (cond [(noroute? route) route]
                      [else (cons state route)]))]))
     
     ;; (find-route/list lostate visited) searches from every state in lostate
     ;;   looking for a path to a solution, accumulating visited states
     ;; find-route/list: (listof X) (listof X) -> (anyof NoRoute (listof X))     
     (define (find-route/list lostate visited)
       (cond [(empty? lostate) (make-noroute visited)]
             [(member (first lostate) visited)
              (find-route/list (rest lostate) visited)]
             [else
              (local [(define route (find-route/acc (first lostate) 
                                                    visited))]
                (cond [(noroute? route) 
                       (find-route/list (rest lostate)
                                        (noroute-visited route))]
                      [else route]))]))
     
     (define route (find-route/acc initial-state empty))]
    
    (cond [(noroute? route) false]
          [else route])))


;; (find-final initial-state neighbours solved?) behaves exactly the same as
;;   find-route (see above) except that if a solution is found, then only
;;   the final state is produced
;; find-route: X (X -> (listof X)) (X -> Bool) -> (anyof false X)
(define (find-final initial-state neighbours solved?)
  (local
    [;; (find-final/single state) searches outward from state 
     ;;   looking for a solution
     ;; find-final/single: X -> (anyof false X)
     (define (find-final/single state)
       (cond [(solved? state) state]
             [else (find-final/list (neighbours state))]))
     
     ;; (find-final/list lostate) searches from every state in lostate
     ;;   looking for a solution
     ;; find-final/list: (listof X) -> (anyof false X)
     (define (find-final/list lostate)
       (cond [(empty? lostate) false]
             [else
              (local [(define fresult (find-final/single (first lostate)))]
                (cond [(false? fresult) (find-final/list (rest lostate))]
                      [else fresult]))]))]
    
    (find-final/single initial-state)))


;; (disp los) displays a list of strings los in the interactions window
;; disp: (listof Str) -> Void
;; effects: displays result in the interactions window
(define (disp los)
  (display (apply string-append (map (lambda (s) 
                                       (string-append s "\n")) los))))


;; (show text) displays text in a Racket slideshow (separate window)
;; show: (listof (listof Str)) -> Void
;; effects: displays slideshow
(define (show text)
  (apply slide (list 'alts (map (lambda (los) (map tt los)) text))))


;; lists-equiv? is not really part of this assignment,
;; but generally useful in tests where we don't care about ordering.
;; The approach is a bit sneaky, but very succinct: Check that
;; every element of l1 appears somewhere in l2 (in terms of equal?),
;; and that every elements of l2 appears somewhere in l1.

;; (lists-equiv? l1 l2) determines whether the l1 and l2 are "equivalent"
;;   and are essentially the same up to reordering.
;; lists-equiv?: (listof Any) (listof Any) -> Bool
;; requires: elements of l1 (or l2) are unique (no duplicates)
(define (lists-equiv? l1 l2)
  (and (= (length l1) (length l2))
       (andmap (lambda (x1) (ormap (lambda (x2) (equal? x1 x2)) l2)) l1)
       (andmap (lambda (x2) (ormap (lambda (x1) (equal? x1 x2)) l1)) l2)))
