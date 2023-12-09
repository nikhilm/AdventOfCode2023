#lang racket

(require advent-of-code)

; the type of the hand can be calculated once
; the comparison between two hands has a fast path if the types are different
; otherwise it has a slow path.

; for comparison
(define five-of-a-kind 6)
(define four-of-a-kind 5)
(define full-house 4)
(define three-of-a-kind 3)
(define two-pair 2)
(define one-pair 1)
(define high-card 0)

(struct hand (cards type) #:transparent)
(struct deal (hands bid) #:transparent)

(define (read-hand)
  (string->list (read-string 5)))

(define (read-bid)
  (begin0
    (read)
    (read-line)))

(define (read-deal)
  (let* ([h (read-hand)]
         [b (read-bid)])
    (deal (make-hand h) b)))

(define (classify-5-cards sorted)
  ; Thanks to bogdanp for this piece of match wizardry.
  ; sort in reverse.
  (match sorted
    [(list _) five-of-a-kind]
    [(list `(,_ . 4) `(,_ . 1)) four-of-a-kind]
    [(list `(,_ . 3) `(,_ . 2)) full-house]
    [(list `(,_ . 3) `(,_ . 1) `(,_ . 1)) three-of-a-kind]
    [(list `(,_ . 2) `(,_ . 2) `(,_ . 1)) two-pair]
    [(list `(,_ . 2) `(,_ . 1) `(,_ . 1) `(,_ . 1)) one-pair]
    [(list _ _ _ _ _) high-card]))

(define (classify-without-jacks sorted)
  ; if we remove the jacks from consideration
  ; assuming there is only 1 jack
  ; then, if we have 4 of the same rank, the jack should become that rank to get to five-of-a-kind
  ; 3, 1 -> become four of a kind
  ; 2, 2 -> make the jack either of them would lead to a full-house
  ; 2, 1, 1 -> get to three-of-a-kind
  ; 1, 1, 1, 1 -> one pair
  ; can never be just high-card
  ;
  ; if there are 2 jacks
  ; 3 of the same rank -> five-of-a-kind
  ; 2, 1 -> four-of-a-kind
  ; 1, 1, 1 -> three-of-a-kind
  ;
  ; if there are 3 jacks
  ; 2 of the same rank -> five of a kind
  ; 1, 1 -> four of a kind
  ;
  ; 4 jacks
  ; five of a kind
  ;
  ; 5 jacks
  ; five of a kind
  (match sorted
    [(or (list) (list _)) five-of-a-kind]
    [(list _ `(,_ . 1)) four-of-a-kind]
    [(list `(,_ . 2) `(,_ . 2)) full-house]
    [(list _ `(,_ . 1) `(,_ . 1)) three-of-a-kind]
    [(list _ _ _ _) one-pair]))

(define (classify hand-chars)
  (define sorted (count-and-sort hand-chars))
  (classify-5-cards sorted))

(define (count-and-sort hand-chars)
  (define counts
    (for/fold ([counts (hash)])
              ([c (in-list hand-chars)])
      (hash-update counts
                   c
                   add1
                   0)))

  (sort (hash->list counts)
        >
        #:key cdr))

(define (classify-2 hand-chars)
  (define sorted (count-and-sort hand-chars))
  (define without-jacks (filter
                         (lambda (it) (not (equal? (car it) #\J)))
                         sorted))
  (if (equal? (length without-jacks) (length sorted))
      (classify-5-cards without-jacks)
      (classify-without-jacks without-jacks)))

(define (make-hand hand-chars)
  (hand hand-chars (classify hand-chars)))

(define (make-hand-2 hand-chars)
  (hand hand-chars (classify-2 hand-chars)))

(define (card-rank card)
  (case card
    [(#\2) 2]
    [(#\3) 3]
    [(#\4) 4]
    [(#\5) 5]
    [(#\6) 6]
    [(#\7) 7]
    [(#\8) 8]
    [(#\9) 9]
    [(#\T) 10]
    [(#\J) 11]
    [(#\Q) 12]
    [(#\K) 13]
    [(#\A) 14]))

(define (card-rank-2 card)
  (case card
    [(#\J) 1]
    [(#\2) 2]
    [(#\3) 3]
    [(#\4) 4]
    [(#\5) 5]
    [(#\6) 6]
    [(#\7) 7]
    [(#\8) 8]
    [(#\9) 9]
    [(#\T) 10]
    [(#\Q) 12]
    [(#\K) 13]
    [(#\A) 14]))

; can be passed as a comparator to sort
(define (compare-hands h1 h2 [rank card-rank])
  (cond
    [(= (hand-type h1) (hand-type h2))
     ; the sophisticated comparison
     (for/first ([h1c (in-list (hand-cards h1))]
                 [h2c (in-list (hand-cards h2))]
                 #:when (not (equal? h1c h2c)))
       (< (rank h1c) (rank h2c)))]
    [else (< (hand-type h1) (hand-type h2))]))

(define (calculate-winnings deals)
  (define sorted-deals
    (sort deals
          compare-hands
          #:key deal-hands))
  (for/sum ([(deal index) (in-indexed (in-list sorted-deals))])
    (* (deal-bid deal) (add1 index))))

(define (run)
  (define deals
    (let loop ([deals null])
      (if (eof-object? (peek-char))
          deals
          (loop (cons (read-deal) deals)))))
  ; reclassify.
  (define deals-2
    (for/list ([d (in-list deals)])
      (deal (make-hand-2 (hand-cards (deal-hands d))) (deal-bid d))))
  (values (calculate-winnings deals) (calculate-winnings deals-2)))

(module+ main
  (parameterize ([current-input-port (open-aoc-input (find-session) 2023 7 #:cache #t)])
    (define-values (p1 p2) (run))
    (printf "Part 1: ~v~n" p1)
    (printf "Part 2: ~v~n" p2)))

(module+ test
  (require rackunit)
  (define-values (p1 p2)
    (with-input-from-string "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
" run))
  (check-equal? p1 6440)
  (check-equal? p2 5905))