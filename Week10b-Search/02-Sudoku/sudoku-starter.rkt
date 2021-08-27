;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sudoku-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require racket/list)  ;gets list-ref, take and drop
        
;; sudoku-starter.rkt

;;
;; Brute force Sudoku solver
;;
;; In Sudoku, the board is a 9x9 grid of SQUARES.
;; There are 9 ROWS and 9 COLUMNS, there are also 9
;; 3x3 BOXES.  Rows, columns and boxes are all UNITs.
;; So there are 27 units.
;;
;; The idea of the game is to fill each square with
;; a Natural[1, 9] such that no unit contains a duplicate
;; number.
;;

;; =================
;; Data definitions:


;; Val is Natural[1, 9]

;; Board is (listof Val|false)   that is 81 elements long
;; interp.
;;  Visually a board is a 9x9 array of squares, where each square
;;  has a row and column number (r, c).  But we represent it as a
;;  single flat list, in which the rows are layed out one after
;;  another in a linear fashion. (See interp. of Pos below for how
;;  we convert back and forth between (r, c) and position in a board.)


;; Pos is Natural[0, 80]
;; interp.
;;  the position of a square on the board, for a given p, then
;;    - the row    is (quotient p 9)
;;    - the column is (remainder p 9)


;; Unit is (listof Pos) of length 9
;; interp. 
;;  The position of every square in a unit. There are
;;  27 of these for the 9 rows, 9 columns and 9 boxes.


;; =================
;; Constants:
;; =================

;=============================================
;;                  [1] Vals
;=============================================
(define ALL-VALS (list 1 2 3 4 5 6 7 8 9))

(define B false) ;B stands for blank

;=============================================
;;                  [2] Board
;=============================================

(define BD1 
  (list B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B))

(define BD2 
  (list 1 2 3 4 5 6 7 8 9 
        B B B B B B B B B 
        B B B B B B B B B 
        B B B B B B B B B 
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B))

(define BD3 
  (list 1 B B B B B B B B
        2 B B B B B B B B
        3 B B B B B B B B
        4 B B B B B B B B
        5 B B B B B B B B
        6 B B B B B B B B
        7 B B B B B B B B
        8 B B B B B B B B
        9 B B B B B B B B))

(define BD4                ;easy
  (list 2 7 4 B 9 1 B B 5
        1 B B 5 B B B 9 B
        6 B B B B 3 2 8 B
        B B 1 9 B B B B 8
        B B 5 1 B B 6 B B
        7 B B B 8 B B B 3
        4 B 2 B B B B B 9
        B B B B B B B 7 B
        8 B B 3 4 9 B B B))

(define BD4s               ;solution to 4
  (list 2 7 4 8 9 1 3 6 5
        1 3 8 5 2 6 4 9 7
        6 5 9 4 7 3 2 8 1
        3 2 1 9 6 4 7 5 8
        9 8 5 1 3 7 6 4 2
        7 4 6 2 8 5 9 1 3
        4 6 2 7 5 8 1 3 9
        5 9 3 6 1 2 8 7 4
        8 1 7 3 4 9 5 2 6))

(define BD5                ;hard
  (list 5 B B B B 4 B 7 B
        B 1 B B 5 B 6 B B
        B B 4 9 B B B B B
        B 9 B B B 7 5 B B
        1 8 B 2 B B B B B 
        B B B B B 6 B B B 
        B B 3 B B B B B 8
        B 6 B B 8 B B B 9
        B B 8 B 7 B B 3 1))

(define BD5s               ;solution to 5
  (list 5 3 9 1 6 4 8 7 2
        8 1 2 7 5 3 6 9 4
        6 7 4 9 2 8 3 1 5
        2 9 6 4 1 7 5 8 3
        1 8 7 2 3 5 9 4 6
        3 4 5 8 9 6 1 2 7
        9 2 3 5 4 1 7 6 8
        7 6 1 3 8 2 4 5 9
        4 5 8 6 7 9 2 3 1))

(define BD6                ;hardest ever? (Dr Arto Inkala)
  (list B B 5 3 B B B B B 
        8 B B B B B B 2 B
        B 7 B B 1 B 5 B B 
        4 B B B B 5 3 B B
        B 1 B B 7 B B B 6
        B B 3 2 B B B 8 B
        B 6 B 5 B B B B 9
        B B 4 B B B B 3 B
        B B B B B 9 7 B B))

(define BD7                 ; no solution 
  (list 1 2 3 4 5 6 7 8 B 
        B B B B B B B B 2 
        B B B B B B B B 3 
        B B B B B B B B 4 
        B B B B B B B B 5
        B B B B B B B B 6
        B B B B B B B B 7
        B B B B B B B B 8
        B B B B B B B B 9))

;=============================================
;;                  [3] Pos
;=============================================

(define P-BD4s-60 60) ;This is the 61st square on the board BD4s
;                      Row index 6 (Row 7), Column index 6 (Col 7)
;                      Val = 1

;=============================================
;;           [4] Units (list of Pos)
;=============================================

;; Positions of all the rows, columns and boxes:

(define ROWS
  (list (list  0  1  2  3  4  5  6  7  8)        ;ROW 1
        (list  9 10 11 12 13 14 15 16 17)        ;ROW 2
        (list 18 19 20 21 22 23 24 25 26)
        (list 27 28 29 30 31 32 33 34 35)
        (list 36 37 38 39 40 41 42 43 44)
        (list 45 46 47 48 49 50 51 52 53)
        (list 54 55 56 57 58 59 60 61 62)
        (list 63 64 65 66 67 68 69 70 71)
        (list 72 73 74 75 76 77 78 79 80)))      ;ROW 9

(define COLS
  (list (list 0  9 18 27 36 45 54 63 72)         ;COL 1 (look at the position of each square in the 1st column)
        (list 1 10 19 28 37 46 55 64 73)
        (list 2 11 20 29 38 47 56 65 74)
        (list 3 12 21 30 39 48 57 66 75)
        (list 4 13 22 31 40 49 58 67 76)
        (list 5 14 23 32 41 50 59 68 77)
        (list 6 15 24 33 42 51 60 69 78)
        (list 7 16 25 34 43 52 61 70 79)
        (list 8 17 26 35 44 53 62 71 80)))       ;COL 9

(define BOXES
  (list (list  0  1  2  9 10 11 18 19 20)        ;Box 1 (1st 3x3 box, Top left side of the board) So, set of 9 (row, col) are
        ;                                         {(0, 0), (0, 1), (0, 2)}
        ;                                         {(1, 0), (1, 1), (1, 2)}
        ;                                         {(2, 0), (2, 1), (2, 2)}
        (list  3  4  5 12 13 14 21 22 23)
        (list  6  7  8 15 16 17 24 25 26)
        (list 27 28 29 36 37 38 45 46 47)
        (list 30 31 32 39 40 41 48 49 50)
        (list 33 34 35 42 43 44 51 52 53)
        (list 54 55 56 63 64 65 72 73 74)
        (list 57 58 59 66 67 68 75 76 77)
        (list 60 61 62 69 70 71 78 79 80)))

(define UNITS (append ROWS COLS BOXES))         ;Units for a Sudoku Board = list of ROWS, COLS and BOXES


;; =================
;;    Functions:
;; =================


;========================================================================

;; Board -> Board or false
;; Given a Sudoku Board bd, produce a solution for it, or false if it is unsolvable
;; Assumption: bd is a valid board

;; Approach:
;; Starting with a board, we find the first empty square, and generate a list of valid
;; next boards. If there is no valid next board and the board is not full, then return false
;; Otherwise, if the board is full and valid, return the board.
;; This is an operation on an arbitrary arity tree, because each board can have 0-9 valid next boards (breadth)
;; and each of those boards can have 0-9 next ones (i.e. depth)

;; (check-expect (solve  BD4)  BD4s)
(check-expect (solve  BD5)  BD5s)
(check-expect (solve BD5s)  BD5s)
(check-expect (solve  BD7) false)

;(define (solve bd) false)

;; Solve Board consists of:
;; [1] Is the board solved?
;; [2] If yes, then return the board
;; [3] If no, then generate the next set of "valid" boards (ListOf Board) and check those
;; While checking a (ListOf Board)
;; [1] If the list of boards is empty, then it implies that there are no further
;;     valid boards. That means this tree of boards does not have the solution, and hence we return false
;; [2] Otherwise,
;;     [2.1] check if the first board in the list gives the solution, i.e. it does not return false
;;     [2.2] if it does, then return that solution
;;     [2.3] otherwise, check the rest of the (ListOf Board) for the presence of a solution

(define (solve bd)
  (local [(define (solve--bd bd)
            (if (solved? bd)
                bd
                (solve--lobd (next-boards bd))))
          (define (solve--lobd lobd)
            (cond [(empty? lobd) false]
                  [else (local [(define check-first-board
                                  (solve--bd (first lobd)))]
                          (if (not (false? check-first-board))
                              check-first-board
                              (solve--lobd (rest lobd))))]))]
    (solve--bd bd)))

;========================================================================

;========================================================================

;; Board -> Boolean
;; Given a Sudoku Board, determine if it is solved or not.
;; Assume: A newly generated board is valid, so if it is full, then it is solved

(check-expect (solved?  BD5) false)
(check-expect (solved? BD5s)  true)

;(define (solved? bd) false) ;stub

(define (solved? bd)
  (andmap number? bd))   ;We're checking if every element of the board is a number. If there is anything that isn't (i.e. false), then the board is not solved

;========================================================================

;========================================================================

;; Board -> ListOf Board
;; Given a Sudoku board, generate a list of valid child boards for it
;; Finds 1st empty square, fills it with Natural[1-9], and keeps only the valid boards
;; Assume: the given board is valid -- come back to this

(check-expect (next-boards (cons 1 (rest BD1)))
              (list (cons 1 (cons 2 (rest (rest BD1))))  ;Note that (list 1 1 (rest (rest BD1))) is invalid, because 1 row cannot have 2 instances of the number 1. Hence, start with 2
                    (cons 1 (cons 3 (rest (rest BD1))))
                    (cons 1 (cons 4 (rest (rest BD1))))
                    (cons 1 (cons 5 (rest (rest BD1))))
                    (cons 1 (cons 6 (rest (rest BD1))))
                    (cons 1 (cons 7 (rest (rest BD1))))
                    (cons 1 (cons 8 (rest (rest BD1))))
                    (cons 1 (cons 9 (rest (rest BD1))))
                    ))


;; (define (next-boards bd) empty)

;; [1] Find the 1st empty square
;; [2] Create 9 new boards by taking the previous board and filling the position from [1] with 1-9
;; [3] Of the 9 created boards, keep only the valid boards

(define (next-boards bd)
  (keep-valid-boards (create-boards-with-1-9 (find-empty bd) bd)))

;========================================================================

;========================================================================

;; [1]
;; Board -> Pos
;; Given a board, find the first empty square
;; Assume: The board has at least 1 empty square

(check-expect (find-empty BD4) 3)
(check-expect (find-empty (cons 2 (cons 4 (rest (rest BD1))))) 2)

;(define (find-empty bd) 0)

(define (find-empty bd)
  (cond [(empty? bd) (error "The board didn't have an empty square")]
        [else
         (if (false? (first bd))                  ;At whatever point we are in the list, if the first element is false, then
             0                                    ;give 0
             (+ 1 (find-empty (rest bd))))]))    ;otherwise, add 1 to the recursive call
;                                                 ;[e.g. 3rd element = 2nd Pos. Call1 => 1+  Call2=> 1+ Call3=> 0(got false); So, total = 2]

;========================================================================

;========================================================================

;; [2]
;; Pos Board -> ListOf Board
;; Given a position and a board, produce a list of 9 boards, with the position filled with natural [1-9]

(check-expect (create-boards-with-1-9 1 (cons 1 (rest BD1)))
              (list (cons 1 (cons 1 (rest (rest BD1))))  
                    (cons 1 (cons 2 (rest (rest BD1))))  
                    (cons 1 (cons 3 (rest (rest BD1))))
                    (cons 1 (cons 4 (rest (rest BD1))))
                    (cons 1 (cons 5 (rest (rest BD1))))
                    (cons 1 (cons 6 (rest (rest BD1))))
                    (cons 1 (cons 7 (rest (rest BD1))))
                    (cons 1 (cons 8 (rest (rest BD1))))
                    (cons 1 (cons 9 (rest (rest BD1))))
                    ))

;(define (create-boards-with-1-9 p bd) empty)

(define (create-boards-with-1-9 p bd)
  (local [(define (create-board-with val) (fill-square bd p val))]     
    (map create-board-with (build-list 9 (lambda (x) (+ x 1))))))    ;We're building a list of natural numbers from 1 till 9 using a "lambda" function combined with "build-list" function
;                                                                    ;We then use the "map" function to use each value from the built list as an argument to local function "create-board-with"
;                                                                    ;which is basically a wrapper over the call to function "fill-square"

;========================================================================

;========================================================================

;; [3]
;; (ListOf Board) -> (ListOf Board)
;; Given a list of boards, return the list of valid boards


(check-expect (keep-valid-boards (list (cons 1 (cons 1 (rest (rest BD1)))))) empty)

;(define (keep-valid-boards lobd) empty)

;; [1] Find if a board in the list is valid
;; [2] Filter the list of boards using the above criteria

(define (keep-valid-boards lobd)
  (filter is-board-valid lobd))

;========================================================================

;========================================================================

;; Board -> Boolean
;; Given a Sudoku board, return true if no unit of the board has the same value twice

(check-expect (is-board-valid (cons 1 (cons 1 (rest (rest BD1))))) false)
(check-expect (is-board-valid (cons 1 (cons 2 (rest (rest BD1))))) true)


;(define (is-board-valid bd) false)

;; For a board
;; [1] Go through the UNITS, or (ListOf units)
;; [2] For each unit in the list of units,
;; [3] map the unit, or list of pos, to a list of values
;; [4] for each list of values, get the list of counts of each value
;; [5] for each list of counts, return true if all counts are valid
;; [6] The final Boolean is determined by taking an AND of all of the above from [5]

(define (is-board-valid bd)
  (local [(define (are-units-valid? lou)                                   ;(ListOf Unit) -> Boolean
            (andmap is-unit-valid? lou))
          
          (define (is-unit-valid? u)                                       ;(Unit), i.e. (ListOf Pos) -> Boolean
            (all-counts-correct?
             (get-list-of-counts
              (map get-val u))))    ;((Position -> Val) (ListOf Pos)) -> (ListOf Val) -> ListOf Integers -> Boolean

          (define (get-val p)                                              ;Position -> Val
            (read-square bd p))]

    (are-units-valid? UNITS)))

;========================================================================

;========================================================================

;; ListOf Val -> ListOf Integer
;; Given a list of val, i.e. Natural[1, 9]/False, convert it to a list of counts of each value in the list, with all values = false having count 0

(check-expect (get-list-of-counts (list 1 2 3 4 5 5 7 7 7)) (list 1 1 1 1 2 2 3 3 3))
(check-expect (get-list-of-counts (list 1 2 3 4 B B 7 7 7)) (list 1 1 1 1 3 3 3))

(define (get-list-of-counts Lst)
  (local [(define (get-count list-val)
            (count (lambda (x) (= x list-val)) only-values))
          (define only-values (filter number? Lst))]
    (map get-count only-values)))    ;We first ignore all false values, and then take count of the values in the reduced list

;========================================================================

;========================================================================

;; ListOf Integer -> Boolean
;; Given a list of counts of Vals (from a unit of a board), return True if each count <= 1; otherwise, return False

(check-expect (all-counts-correct? (list 1 1 1 1 2 2 3 3 3)) false)
(check-expect (all-counts-correct? (list 1 1 1 1 0 0 1 0 1))  true)

(define (all-counts-correct? Lst)
  (andmap (lambda (x) (<= x 1)) Lst))

;========================================================================

;========================================================================
;; Natural[0, 8] Natural[0, 8] -> Pos
;; Convert 0-based row and column to Pos
(check-expect (r-c->pos 0 0) 0)
(check-expect (r-c->pos 6 6) 60)

(define (r-c->pos r c) (+ (* r 9) c))  ;helpful for writing tests
;========================================================================

;========================================================================
;; Board Pos -> Val or false
;; Produce value at given position on board.
(check-expect (read-square BD2 (r-c->pos 0 5)) 6)  ;Get the value from square at row-index 0, column-index 5 of Board 2
(check-expect (read-square BD3 (r-c->pos 7 0)) 8)  ;Get the value from square at row-index 7, column-index 0 of Board 3

(define (read-square bd p)
  (list-ref bd p))               
;========================================================================

;========================================================================
;; Board Pos Val -> Board
;; produce new board with val at given position
(check-expect (fill-square BD1 (r-c->pos 0 0) 1)  ;Put the value 1 in the square at position 0 (row-index 0, column-index 0)
              (cons 1 (rest BD1)))                ;create a list with the value 1 followed by the rest of BD1 (i.e. the first BD1 is replaced by 1)

(define (fill-square board position new-val)
  (append (take board position)                   ;Make a list of the 1st <position> elements of the board
          (list new-val)                          ;Create a list of the new value to be added
          (drop board (add1 position))))          ;Add one to the position, and create a list from that till the end
;                                                 ;Append the above 3 lists.
;                                                 ;So, if position = 20, then get the list from 0-20, then put the new value in a list, and then get the list from from 21-80, and finally append all 3
;========================================================================