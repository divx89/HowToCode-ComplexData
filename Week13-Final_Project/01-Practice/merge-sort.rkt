;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname merge-sort) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; merge-sort

;; (listOf Number) -> (listOf Number)
;; Sort a list of numbers in ascending order using merge sort

(check-expect (merge-sort (list                )) (list                ))
(check-expect (merge-sort (list               1)) (list               1))
(check-expect (merge-sort (list             4 5)) (list             4 5))
(check-expect (merge-sort (list             5 4)) (list             4 5))
(check-expect (merge-sort (list           9 1 5)) (list           1 5 9))
(check-expect (merge-sort (list 8 12 14 1 3 5 2)) (list 1 2 3 5 8 12 14))

;Merge sort functions by splitting the list into 2 parts (as equal as possible)
;until at-least one part cannot be split further
;Each split part is merged with the next one by sorting the elements on from the beginning
;e.g.
;      8 2 5 4
;     8 2   5 4
;    8   2 5   4
;     2 8   4 5
;      2 4 5 8

(define (merge-sort lon)
  (cond [(or (empty? (rest lon)) (empty? lon)) lon]
        [else (merge (merge-sort (get-part lon 1))
                     (merge-sort (get-part lon 2)))]))

;; (listOf Number) Integer[1,2] -> (listOf Number)
;; Given a (listOf Number) and a part number (1/2), divide the lon to return the 1st or the 2nd half

(check-expect (get-part (list               8) 1) (list         8))
(check-expect (get-part (list               8) 2) (list          ))
(check-expect (get-part (list   5 3 9 7 2 1 8) 1) (list 5  3  9 7))
(check-expect (get-part (list   5 3 9 7 2 1 8) 2) (list    2  1 8))

(define (get-part lon0 part-num)
  (local [(define half-l-lon0 (/ (length lon0) 2))
                  
          (define new-length (cond [(= part-num 1) (ceiling half-l-lon0)]
                                   [else (floor half-l-lon0)]))
          
          (define (create lon new-lon pos)
            (cond [(= (length new-lon) new-length) new-lon]
                  [else (create lon (append new-lon (list (list-ref lon pos))) (add1 pos))]))]
    (create lon0 empty (if (= part-num 1) 0 (ceiling half-l-lon0)))))


;; (listOf Number) (listOf Number) -> (listOf Number)
;; Merge two lists of numbers by comparing the first elemente of each

(check-expect (merge (list     2) (list     1)) (list       1 2))
(check-expect (merge (list     2) (list   1 3)) (list     1 2 3))
(check-expect (merge (list   2 8) (list     3)) (list     2 3 8))
(check-expect (merge (list   2 8) (list   4 5)) (list   2 4 5 8))
(check-expect (merge (list   2 8) (list 1 3 9)) (list 1 2 3 8 9))
(check-expect (merge (list 1 3 9) (list   2 8)) (list 1 2 3 8 9))

(define (merge lon-01 lon-02)
  (local [(define (fn-for-lon lon1 lon2 merged-lon)
            (cond [(or (empty? lon1) (empty? lon2)) (append merged-lon lon1 lon2)]
                  [(<= (first lon1) (first lon2)) (fn-for-lon (rest lon1) lon2 (append merged-lon (list (first lon1))))]
                  [else (fn-for-lon lon1 (rest lon2) (append merged-lon (list (first lon2))))]))]
    (fn-for-lon lon-01 lon-02 empty)))