#lang typed/racket/optional

(require/typed advent-of-code
               [open-aoc-input (-> Any Any Any [#:cache Boolean] Input-Port)]
               [find-session (-> Any)])

(struct game
  ([id : Positive-Integer]
   [winning : (Setof Positive-Integer)]
   [have : (Setof Positive-Integer)])
  #:transparent)

(define (str-numbers->set (input : String))
  (for/set : (Setof Positive-Integer)
    ([token (in-list (string-split (string-trim input)))])
    (assert (string->number token) exact-positive-integer?)))

(define (win-count (g : game)) exact-nonnegative-integer?
  (set-count (set-intersect (game-winning g) (game-have g))))

(: run (-> Void))
(define (run)
  (define games
    (for/list : (Listof game) ([line (in-lines)])
      #;(printf "Line: ~v~n" line)
      (define pieces (regexp-match #px"Card\\s+([0-9]+): (.*) \\| (.*)" line))
      #;(printf "Pieces are ~v~n" pieces)
      (let ([pieces (cast pieces (List String String String String))])
        (game
         (assert (string->number (second pieces)) exact-positive-integer?)
         (str-numbers->set (third pieces))
         (str-numbers->set (fourth pieces))))))
  #;(printf "Games: ~a~n" games)

  (printf "Part 1: ~a~n"
          (for/sum ([g (in-list games)])
            (define num-winning (win-count g))
            (define score (if (= 0 num-winning) 0 (expt 2 (sub1 num-winning))))
            #;(printf "Score ~a: ~a~n" (game-id g) score)
            score))

  ; create a hash from card id to number of cards
  (define counts-by-id
    ; use copy to make it mutable
    (hash-copy
     (for/hash : (HashTable Positive-Integer Positive-Integer) ([g (in-list games)])
       (values (game-id g) 1))))
  ; then play forward and update the number of cards. use that to determine total cards acquired.
  (for ([g (in-list games)])
    (define num-winning (win-count g))
    (define start-id (add1 (game-id g)))
    (define current-count (hash-ref counts-by-id (game-id g)))
    ; add 1 card for each of the next num-winning ids
    (for ([id (in-range start-id (+ start-id num-winning))])
      (hash-update! counts-by-id
                    (assert id positive-integer?)
                    (lambda ([existing : Positive-Integer]) : Positive-Integer
                      (+ existing current-count)))))
  #;(printf "card totals ~v~n" counts-by-id)
  (printf "Part 2: ~a~n"
          (for/sum ([v (in-hash-values counts-by-id)])
            v)))

(module+ main
  (parameterize ([current-input-port (open-aoc-input (find-session) 2023 4 #:cache #t)])
    (run)))

(module+ test
  (with-input-from-string
      "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
    run))