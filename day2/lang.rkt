#lang br/quicklang

(struct game (id sets) #:transparent)

(define (possible-set? set)
  (and (<= (hash-ref set "red" 0) 12)
       (<= (hash-ref set "green" 0) 13)
       (<= (hash-ref set "blue" 0) 14)))

(define (possible? game)
  (for/and ([set (in-list (game-sets game))])
    (possible-set? set)))

(define (fewest g)
  (for/fold ([minimums (hash)])
            ([set (in-list (game-sets g))])
    (for/fold ([min-for-color minimums])
              ([(color value) (in-hash set)])
      (hash-update min-for-color
                   color
                   (lambda (min-so-far)
                     (max min-so-far value))
                   (lambda ()
                     value)))))

(define (power g)
  (for/product ([fewest-for-color (in-hash-values (fewest g))])
    fewest-for-color))

(define (read-syntax path port)
  (define games
    (for/list ([line (in-lines port)]
               #:when (non-empty-string? line))
      (define match-range (second (regexp-match-positions #rx"Game ([0-9]+):" line)))
      (define game-id (string->number (substring line (car match-range) (cdr match-range))))
      (define rest (string-trim (substring line (add1 (cdr match-range)))))
      #;(printf "game-id ~a rest ~a~n" game-id rest)
      (define sets
        (for/list ([set-str (in-list (string-split rest ";"))])
          (for/hash ([reveal (in-list (string-split set-str ","))])
            (define set-match (regexp-match #rx"([0-9]+) (red|green|blue)" reveal))
            (values (third set-match) (string->number (second set-match))))))
      (game game-id sets)))
  ; This isn't the best because it isn't preserving any syntax/datum information from the parse.
  (define module-datum `(module day1-mod "lang.rkt"
                          (handle-args ,@games)))
  (datum->syntax #f module-datum))
(provide read-syntax)

(define-macro (day1-module-begin HANDLE-ARGS-EXPR)
  #'(#%module-begin
     HANDLE-ARGS-EXPR))
(provide (rename-out [day1-module-begin #%module-begin]))

(define (handle-args . games)
  (printf "part 1 ~v~n"
          (for/sum ([g (in-list games)])
            (if (possible? g)
                (game-id g)
                0)))
  (printf "part 2 ~v~n"
          (for/sum ([g (in-list games)])
            (power g))))
(provide handle-args)
