#lang br/quicklang

(struct game (id sets) #:transparent)

(define (possible-set? set)
  (and (<= (hash-ref set "red" 0) 12)
       (<= (hash-ref set "green" 0) 13)
       (<= (hash-ref set "blue" 0) 14)))

(define (possible? game)
  (for/and ([set (in-list (game-sets game))])
    (possible-set? set)))

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
  (printf "part 1 ~v~n"
          (for/sum ([g (in-list games)])
            (printf "game ~a, possible? ~a~n" (game-id g) (possible? g))
            (if (possible? g)
                (game-id g)
                0)))
  #;(define src-datums (format-datums '~s src-lines))
  (define module-datum `(module day1-mod "lang.rkt"
                          (handle-args)))
  (datum->syntax #f module-datum))
(provide read-syntax)

(define-macro (day1-module-begin HANDLE-ARGS-EXPR)
  #'(#%module-begin
     (display HANDLE-ARGS-EXPR)))
(provide (rename-out [day1-module-begin #%module-begin]))

(define (handle-args . args)
  void)
(provide handle-args)
