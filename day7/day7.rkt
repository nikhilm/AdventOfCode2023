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
  (string->list (symbol->string (read))))

(define (read-bid)
  (begin0
    (read)
    (read-line)))

(define (classify hand-chars)
  )

(define (make-hand hand-chars)
  (hand hand-chars (classify hand-chars)))

(define (read-deal)
  (let* ([h (read-hand)]
         [b (read-bid)])
  (deal (make-hand h) b)))

(define (run)
  (define deals
    (let loop ([deals null])
      (if (eof-object? (peek-char))
          deals
          (loop (cons (read-deal) deals)))))
  (printf "~v~n" deals)
  (values 0 0))

(module+ main
  #;(parameterize ([current-input-port (open-aoc-input (find-session) 2023 7 #:cache #t)])
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
  (check-equal? p2 0))