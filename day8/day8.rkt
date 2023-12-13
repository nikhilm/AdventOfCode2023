#lang racket

(require advent-of-code)

; naive attempt.
; model the tree as a hash table, with the node as the key, then the value a 2-list of left and right edges
; containing the node strings as the values.

(define (read-tree-as-hash)
  (for/hash ([line (in-lines)])
    (match-define (regexp #px"([A-Z0-9]{3}) = \\(([A-Z0-9]{3}), ([A-Z0-9]{3})\\)"
                          (list _ src left right))
      line)
    (values src (list left right))))

(define (path-find-internal tree-hash path start end-condition)
  (for/fold ([cur-node start]
             [count 0]
             #:result count)
            ([direction (in-cycle (in-list path))]
             #:break (end-condition cur-node))
    (values
     (case direction
       [(#\L) (first (hash-ref tree-hash cur-node))]
       [(#\R) (second (hash-ref tree-hash cur-node))])
     (add1 count))))

(define (path-find tree-hash path)
  (path-find-internal tree-hash path "AAA" (lambda (e) (equal? e "ZZZ"))))

(define (ending-in tree-hash ch)
  (sequence-filter (lambda (el) (ends-in? el ch)) (in-hash-keys tree-hash)))

(define (ends-in? node ch)
  (equal? ch (string-ref node (sub1 (string-length node)))))

(define (all? pred lst)
  (for/and ([el (in-list lst)])
    (pred el)))

(define (path-find-amb tree-hash path)
  #;(for/fold ([cur-nodes (sequence->list (ending-in tree-hash #\A))]
             [count 0]
             #:result count)
            ([direction (in-cycle (in-list path))]
             #:break (all? (lambda (el) (ends-in? el #\Z)) cur-nodes))
    (values
     (case direction
       [(#\L) (map (lambda (cur-node) (first (hash-ref tree-hash cur-node))) cur-nodes)]
       [(#\R) (map (lambda (cur-node) (second (hash-ref tree-hash cur-node))) cur-nodes)])
     (add1 count)))
  ;hmm lets try the LCM solution everyone is talking about on Reddit.
  (define a-nodes (sequence->list (ending-in tree-hash #\A)))
  (define individual-solutions
    (map
     (lambda (start) (path-find-internal tree-hash path start (lambda (el) (ends-in? el #\Z))))
     a-nodes))
  (printf "Indiv solutions ~v~n" individual-solutions)
  ; my individual solutions were '(22411 24253 18727 16271 18113 21797)
  ; I plugged it into an online LCM calculator and got 23977527174353
  0)

(define (run)
  (define path (string->list (string-trim (read-line))))
  (read-line)
  (define tree-hash (read-tree-as-hash))

  (printf "Path ~v~n" path)
  (printf "Treerep ~v~n" tree-hash)

  #;(printf "ending in A ~v~n" (sequence-length (ending-in tree-hash #\A)))
  #;(printf "ending in Z ~v~n" (sequence-length (ending-in tree-hash #\Z)))
  ; In my input there are 6 nodes starting in A and ending in Z
  ; starting from all 6, if we stuck to the current way of doing things, we would need
  ; to check whether all nodes were ending in Z after every iteration.
  ; that might be enough?
  (values (path-find tree-hash path) (path-find-amb tree-hash path)))

(module+ main
  (parameterize ([current-input-port (open-aoc-input (find-session) 2023 8 #:cache #t)])
      (define-values (p1 p2) (run))
      (printf "Part 1: ~v~n" p1)
      (printf "Part 2: ~v~n" p2)))

(module+ test
  (require rackunit)
  (let-values ([(p1 p2)
                (with-input-from-string "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)" run)])
    (check-equal? p1 2)
    (check-equal? p2 2))

  (let-values ([(p1 p2)
                (with-input-from-string "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)" run)])
    (check-equal? p1 6)
    (check-equal? p2 6)))