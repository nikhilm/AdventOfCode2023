#lang racket

(require advent-of-code)

(struct race (time record))

(define (read-line-of-numbers)
  (match-define (regexp #rx"(Time|Distance): (.+)"
                        (list _ _ numbers))
    (read-line))
  (map string->number (string-split numbers)))

(define (distance-traveled charge-time race-time)
  (define speed charge-time)
  (* speed (- race-time charge-time)))

(define (ways-to-beat time record)
  (sequence-count (lambda (charge-time)
                    (> (distance-traveled charge-time time) record))
                  ; don't bother with 0 and time itself, which will always move by 0mm.
                  (in-range 1 time)))

(define (concat-entries entry-nums)
  ; yep, this is very inefficient. it doesn't matter.
  (string->number (apply string-append (map number->string entry-nums))))

(define (run)
  (define times (read-line-of-numbers))
  (define records (read-line-of-numbers))

  (define p2time (concat-entries times))
  (define p2record (concat-entries records))
  (printf "p2 inputs ~v ~v~n" p2time p2record)
  (values
   (for/product ([time (in-list times)]
                 [record (in-list records)])
     (ways-to-beat time record))
   (ways-to-beat p2time p2record)))

(module+ main
  (parameterize ([current-input-port (open-aoc-input (find-session) 2023 6 #:cache #t)])
      (define-values (p1 p2) (run))
      (printf "Part 1: ~v~n" p1)
      (printf "Part 2: ~v~n" p2)))

(module+ test
  (require rackunit)
  (define-values (p1 p2)
    (with-input-from-string "Time:      7  15   30
Distance:  9  40  200" run))
  (check-equal? p1 288)
  (check-equal? p2 71503))