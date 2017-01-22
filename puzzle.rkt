;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname puzzle) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following line is REQUIRED (do not remove)
(require "puzlib.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Place your Personal Identification here
;; Qifan Zhu
;; Student ID: 20604698

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

;; A Puzzle is a (list (listof String) (listof String))

;; A Grid is a (listof (listof Char))

(define-struct wpos (row col horiz? len))
;; A WPos (Word Position) is a (make-wpos Nat Nat Bool Nat)
;; requires: len > 1

(define-struct state (grid positions words))
;; A State is a (make-state Grid (listof WPos) (listof Str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS FOR TESTING:

(define puzz01 (read-puzzle "puzzle01.txt"))
(define grid-abc '((#\A #\B #\C) (#\X #\Y #\Z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PROVIDED HELPER:

;; (flip wp) transposes wp by reversing row/col and negating horiz?
;; flip: WPos -> WPos
;; Example:
(check-expect (flip (make-wpos 3 4 true 5))
              (make-wpos 4 3 false 5))

(define (flip wp)
  (make-wpos (wpos-col wp) (wpos-row wp) (not (wpos-horiz? wp)) (wpos-len wp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; REQUIRED FUNCTIONS:

(define puzzle01 '(("#######" "#.#.#.#" "#######" "#.#.#.#" "#######" "#.#.#.#" "#######")
                   ("AVERAGE" "CASSIUS" "CUSTARD" "DESSERT" "IMITATE" "SECTION" "SUCCESS" "SUNBELT")))
(define grid01 '((#\A #\B #\C #\D) (#\X #\Y #\Z #\T) (#\M #\N #\Q #\V)))
;; (transpose g) produces the transpose of the Grid(g)
;; transpose: Grid -> Grid
;; Examples:
(check-expect (transpose empty) empty)
(check-expect (transpose '((#\A #\B))) '((#\A) (#\B)))
(check-expect (transpose grid-abc) '((#\A #\X) (#\B #\Y) (#\C #\Z)))

  (define (transpose g)
  (cond [(or (empty? g) (empty? (first g))) empty]
        [else
          (cons (map first g)
                (transpose (map rest g)))]))

;; Tests:
(check-expect (transpose '((#\A #\B #\C) (#\X #\Y #\Z) (#\M #\N #\Q)))
              '((#\A #\X #\M) (#\B #\Y #\N) (#\C #\Z #\Q)))
(check-expect (transpose '((#\A #\B) (#\X #\Y) (#\M #\N)))
              '((#\A #\X #\M) (#\B #\Y #\N)))

;; (find-wpos loc row) produces a list of all horizontal WPos 
;; that occur in the row from loc
;; find-wpos: (listof Char) Nat -> (listof WPos)
;; Examples:
(check-expect (find-wpos empty 0) empty)
(check-expect (find-wpos (string->list ".###.####..") 5) (list (make-wpos 5 1 true 3)
                                                               (make-wpos 5 5 true 4)))
(check-expect (find-wpos (string->list "####") 0)
              (list (make-wpos 0 0 true 4)))

(define (find-wpos loc row)
  (local [;; (find-words loc n) produces how many letters does
          ;; the first word in loc has with accumulator n
          ;; find-words: (listof Char) Nat -> Nat
          (define (find-words loc n) 
            (cond [(or (empty? loc) (char=? (first loc) #\.)) n]
                  [else
                   (find-words (rest loc) (add1 n))]))
          ;; (find-wpos/apply loc n) produces a list with the length 
          ;; of the word and the rest of loc with accumulator n.
          ;; find-wpos/apply: (listof Char) Nat -> (list Nat (listof Char))
          (define (find-wpos/apply loc n)
            (cond [(empty? loc) empty]
                  [(char=? (first loc) #\#)
                   (find-wpos/apply (rest loc) (add1 n))]
                  [else (cons n loc)]))
          ;; (find-wpos/acc loc row n) produces a list of all horizontal  
          ;; WPos that occur in the row from loc with accumulator n.
          ;; find-wpos/acc: (listof Char) Nat Nat -> (listof WPos)
          (define (find-wpos/acc loc row n)
            (cond [(empty? loc) empty]
                  [(char=? (first loc) #\#)
                   (cons (make-wpos row n true (find-words loc 0))
                         (cond [(empty? (find-wpos/apply loc n)) empty]
                               [else 
                                (find-wpos/acc (rest (find-wpos/apply loc n))
                                               row
                                               (first (find-wpos/apply loc n)))]))]
                  [else
                   (find-wpos/acc (rest loc) row (add1 n))]))]
    (filter (lambda (x) (not (= (wpos-len x) 1))) (find-wpos/acc loc row 0))))

;; Tests:
(check-expect (find-wpos (string->list "###") 5)
              (list (make-wpos 5 0 true 3)))
(check-expect (find-wpos (string->list "..####..") 5)
              (list (make-wpos 5 2 true 4)))
(check-expect (find-wpos (string->list "..#.#.##..") 3)
              (list (make-wpos 3 6 true 2)))


;; the order does not matter: here is an example
;; that uses lists-equiv?
(check-expect (lists-equiv?
               (find-wpos (string->list "..####...###..") 5)
               (list (make-wpos 5 2 true 4)
                     (make-wpos 5 9 true 3)))
              true)
(check-expect (find-wpos (string->list "#.#..#.#") 5)
              empty)


;; (initial-state puzzle) produces the initial State to
;; start searching form from puzzle.
;; initial-state: Puzzle -> State
;; requires: puzzle is non-empty
;;Examples:
(check-expect (initial-state puzz01)
              (make-state (list (list #\# #\# #\#))
                          (list (make-wpos 0 0 true 3))
                          (list "CAT")))

(define (initial-state puzzle)
  (local [;; (puzzle-grid los) produces a Grid from los.
          ;; puzzle-grid: (listof Str) -> Grid
          (define (puzzle-grid los)
            (cond [(empty? los) empty]
                  [else
                   (cons (string->list (first los))
                         (puzzle-grid (rest los)))]))
          (define loloc (puzzle-grid (first puzzle)))
          ;; (puzzle-wpos loloc n) produces a list of WPos from
          ;; loloc with accumulator n.
          ;; puzzle-wpos: Grid Nat -> (listof WPos)
          (define (puzzle-wpos loloc n)
            (cond [(empty? loloc) empty]
                  [else
                   (append (find-wpos (first loloc) n)
                           (puzzle-wpos (rest loloc) (add1 n)))]))
          ;; (flip-lowpos lowpos) produces a listof WPos from lowpos
          ;; been fliped.
          ;; flip-lowpos: (listof WPos) -> (listof WPos)
          (define (flip-lowpos lowpos)
            (cond [(empty? lowpos) empty]
                  [else
                   (cons (flip (first lowpos))
                           (flip-lowpos (rest lowpos)))]))]
    (make-state loloc
                (append (puzzle-wpos loloc 0)
                        (flip-lowpos (puzzle-wpos (transpose loloc) 0)))
                (second puzzle))))

;; Tests:
(check-expect (initial-state puzzle01)
              (make-state (list (list #\# #\# #\# #\# #\# #\# #\#)
                                (list #\# #\. #\# #\. #\# #\. #\#)
                                (list #\# #\# #\# #\# #\# #\# #\#)
                                (list #\# #\. #\# #\. #\# #\. #\#)
                                (list #\# #\# #\# #\# #\# #\# #\#)
                                (list #\# #\. #\# #\. #\# #\. #\#)
                                (list #\# #\# #\# #\# #\# #\# #\#))
                          (list
                           (make-wpos 0 0 true 7)
                           (make-wpos 2 0 true 7)
                           (make-wpos 4 0 true 7)
                           (make-wpos 6 0 true 7)
                           (make-wpos 0 0 false 7)
                           (make-wpos 0 2 false 7)
                           (make-wpos 0 4 false 7)
                           (make-wpos 0 6 false 7))
                          (list "AVERAGE" "CASSIUS" "CUSTARD" "DESSERT" "IMITATE" "SECTION" "SUCCESS" "SUNBELT")))

;; (extract-wpos g wp) produces the (listof Char) corresponding
;; to that word position(wp) within the Grid(g).
;; extract-wpos: Grid WPos -> (listof Char)
;; Examples:
(check-expect (extract-wpos empty (make-wpos 0 0 true 2)) empty)
(check-expect (extract-wpos grid-abc (make-wpos 0 0 true 2)) '(#\A #\B))
(check-expect (extract-wpos grid-abc (make-wpos 0 0 false 2)) '(#\A #\X))
(check-expect (extract-wpos grid-abc (make-wpos 0 2 false 2)) '(#\C #\Z))

(define (extract-wpos g wp)
  (local [;; (extract-row loloc n) produces the n row of the
          ;; Grid(loloc).
          ;; extract-row: Grid Nat -> (listof Char)
          (define (extract-row loloc n)
            (cond [(empty? loloc) empty]
                  [(= n 0) (first loloc)]
                  [else
                   (extract-row (rest loloc) (sub1 n))]))
          ;; (find-loc loc position length) produces a list of Char
          ;; from position and has length of length from loc.
          ;; find-loc: (listof Char) Nat Nat -> (listof Char)
          (define (find-loc loc position length)
            (cond [(empty? loc) empty]
                  [(= length 0) empty] 
                  [(= position 0) 
                   (cons (first loc)
                         (find-loc (rest loc) position (sub1 length)))]
                  [else
                   (find-loc (rest loc) (sub1 position) length)]))]
  (cond [(equal? (wpos-horiz? wp) true)
         (find-loc (extract-row g (wpos-row wp)) (wpos-col wp) (wpos-len wp))]
        [else
         (find-loc (extract-row (transpose g) (wpos-col wp)) (wpos-row wp) (wpos-len wp))])))

;; Tests:
(check-expect (extract-wpos grid01 (make-wpos 1 1 false 2))
              '(#\Y #\N))
(check-expect (extract-wpos grid01 (make-wpos 1 1 true 2))
              '(#\Y #\Z))
(check-expect (extract-wpos grid01 (make-wpos 0 1 false 3))
              '(#\B #\Y #\N))

;; (replace-wpos g wp loc) produces the Grid with the
;; word position(wp) replaced by the word represented
;; by loc from g.
;; replace-wpos: Grid WPos (listof Char) -> Grid
;; requires: len in WPos is equal to length of (listof Char)
;; Examples:
(check-expect (replace-wpos grid-abc (make-wpos 0 0 true 2) '(#\J #\K))
              '((#\J #\K #\C) (#\X #\Y #\Z)))
(check-expect (replace-wpos grid-abc (make-wpos 0 0 false 2) '(#\J #\K))
              '((#\J #\B #\C) (#\K #\Y #\Z)))
(check-expect (replace-wpos grid-abc (make-wpos 0 2 false 2) '(#\J #\K))
              '((#\A #\B #\J) (#\X #\Y #\K)))

(define (replace-wpos g wp loc)
  (local [;; (extract-row loloc n) produces  the n row of the
          ;; Grid(loloc).
          ;; extract-row: Grid Nat -> (listof Char)
          (define (extract-row loloc n)
            (cond [(empty? loloc) empty]
                  [(= n 0) (first loloc)]
                  [else
                   (extract-row (rest loloc) (sub1 n))]))
          ;; (replace loc loc-to-replace position length) produces a new
          ;; (listof Char) that has loc-to-replace to replace loc from
          ;; position to the length of length.
          ;; replace: (listof Char) (listof Char) Nat Nat -> (listof Char)
          (define (replace loc loc-to-replace position length)
            (cond [(empty? loc) empty]
                  [(= length 0) loc]
                  [(= position 0)
                   (cons (first loc-to-replace)
                         (replace (rest loc) (rest loc-to-replace) position (sub1 length)))]
                  [else
                   (cons (first loc)
                         (replace (rest loc) loc-to-replace (sub1 position) length))]))
          ;; (replace-row g loc row) produces a new Grid that g has been
          ;; replaced by loc from row. 
          ;; replace-row: Grid (listof Char) Nat -> Grid
          (define (replace-row g loc row)
            (cond [(= row 0)
                   (cons loc (rest g))]
                  [else
                   (cons (first g)
                         (replace-row (rest g) loc (sub1 row)))]))]
    (cond [(equal? (wpos-horiz? wp) true)
           (replace-row g (replace (extract-row g (wpos-row wp)) loc (wpos-col wp) (wpos-len wp)) (wpos-row wp))]
          [else
           (transpose (replace-row (transpose g)
                                   (replace (extract-row (transpose g) (wpos-col wp)) loc (wpos-row wp) (wpos-len wp))
                                   (wpos-col wp)))])))
                  

;; Tests:
(check-expect (replace-wpos grid01 (make-wpos 0 0 false 3) '(#\J #\K #\I))
              '((#\J #\B #\C #\D) (#\K #\Y #\Z #\T) (#\I #\N #\Q #\V)))
(check-expect (replace-wpos grid01 (make-wpos 1 2 true 2) '(#\J #\K))
              '((#\A #\B #\C #\D) (#\X #\Y #\J #\K) (#\M #\N #\Q #\V)))
(check-expect (replace-wpos grid01 (make-wpos 1 2 false 2) '(#\J #\K))
              '((#\A #\B #\C #\D) (#\X #\Y #\J #\T) (#\M #\N #\K #\V)))

;; (fit? word cells) determines if word can successfully
; be placed in the corresponding word position(cells).
;; fit? (listof Char) (listof Char) -> Bool
;; Examples:
(check-expect (fit? (string->list "STARWARS") (string->list "S##RW##S")) true)
(check-expect (fit? (string->list "STARWARS") (string->list "S##RT##K")) false)

(define (fit? word cells)
  (cond [(and (empty? word) (empty? cells)) true]
        [(or (empty? word) (empty? cells)) false]
        [(or (char=? (first cells) #\#)
             (char=? (first word) (first cells)))
         (fit? (rest word) (rest cells))]
        [else false]))
         
;; Tests:
(check-expect (fit? (string->list "STARWAR") (string->list "S##RW##K")) false)
(check-expect (fit? (string->list "STARWARS") (string->list "S##R####")) true)
(check-expect (fit? (string->list "S") (string->list "#")) true)


;; (neighbours s) produces a list of States, each corresponding 
;; to a different word placed in the grid at the selected WPos in s.
;; neighbours: State -> (listof State)
;; Examples:
(check-expect (neighbours (make-state (list (list #\# #\# #\#))
                          (list (make-wpos 0 0 true 3))
                          (list "CAT")))
              (list (make-state '((#\C #\A #\T)) empty empty)))

(define (neighbours s) 
  (local [;; (count loc n) produces the number of letters that
          ;; loc has with accumulator n. 
          ;; (listof Char) Nat -> Nat
          (define (count loc n)
            (cond [(empty? loc) n]
                  [(char=? (first loc) #\#)
                   (count (rest loc) n)]
                  [else
                   (count (rest loc) (add1 n))]))
          ;; (pick-wpos grid lowpos acc picked-wpos) produces the
          ;; WPos from lowps that has most letter already fills in 
          ;; grid with accumulator acc and picked-wpos.
          ;; pick-wpos: Grid (listof WPos) Nat WPos -> WPos
          (define (pick-wpos grid lowpos acc picked-wpos)
            (cond [(empty? lowpos) picked-wpos]
                  [else (local [(define acc1 (count (extract-wpos grid (first lowpos)) 0))]
                          (cond [(> acc acc1) 
                                 (pick-wpos grid (rest lowpos) acc picked-wpos)]
                                [else
                                 (pick-wpos grid (rest lowpos) acc1 (first lowpos))]))]))
          ;; (take-out string los) produces a list ofstring from los
          ;; without string
          ;; take-out: Str (listof Str) -> (listof Str)
          (define (take-out string los)
            (cond [(string=? (first los) string) (rest los)]
                  [else (cons (first los)
                              (take-out string (rest los)))]))
                   
          ;; (find-state grid wpos los) produces a list of states
          ;; that a word from los has been place into grid in
          ;; word positions(wpos).
          ;; find-state: Grid WPos (listof Str) -> (listof States) 
          (define (find-state grid wpos los)
            (cond [(empty? los) empty]
                  [(fit? (string->list (first los))
                         (extract-wpos grid wpos)) 
                   (cons (make-state (replace-wpos grid wpos (string->list (first los)))
                                     (filter (lambda (x) (not (equal? x wpos))) (state-positions s)) 
                                     (take-out (first los) (state-words s))) 
                         (find-state grid wpos (rest los)))]
                  [else
                   (find-state grid wpos (rest los))]))
          ;; (find-state/list grid lowpos los) produces a list of
          ;; States, each corresponding to a different word in los
          ;; placed in the grid at the selected WPos from lowpos
          ;; find-state/list: Grid (listof WPos) (listof Str) -> (listof States)
          (define (find-state/list grid lowpos los)
            (cond [(empty? lowpos) empty]
                  [else (find-state grid (pick-wpos grid lowpos 0 (first lowpos)) los)]))]
    (find-state/list (state-grid s) (state-positions s) (state-words s)))) 
            
        
;; Tests:
(check-expect (neighbours (make-state '((#\C #\# #\#))
                                      (list (make-wpos 0 0 true 3))
                                      '("CAT" "DOG" "CAR")))
              (list (make-state '((#\C #\A #\T)) empty '("DOG" "CAR"))
                    (make-state '((#\C #\A #\R)) empty '("CAT" "DOG"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PROVIDED FUNCTIONS:

;; (solved? s) determines if s is a solved criss-cross problem
;;   by checking if all of the word positions have been filled
;; solved?: State -> Bool
(define (solved? s)
  (empty? (state-positions s)))


;; (criss-cross puzzle) produces a list of strings corresponding
;;   to the solution of the the criss-cross puzzle,
;;   or false if no solution is possible
;; criss-cross: Puzzle -> (anyof false (listof Str))

(define (criss-cross puzzle)
  (local [(define result (solve (initial-state puzzle)
                                neighbours
                                solved?))]
    (cond [(false? result) false]
          [else (map list->string (state-grid result))])))

 ;(check-expect (criss-cross puzz01) '("CAT"))

;; note that [solve] (used in criss-cross above) is provided in puzlib.rkt

;; when you are all done, you can use disp to
;; view your solutions:

(disp(criss-cross (read-puzzle "puzzle08.txt")))

;; NOTE: Do NOT leave top-level expressions in your code.
;;       In other words, when your code is run, only the
;;       check-expect message "All X tests passed!"
;;       should appear in the interactions window

