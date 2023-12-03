#lang br/quicklang

(require srfi/13)

(define (read-syntax path port)
  (define src-lines (port->lines port))
  (define src-datums (format-datums '~s src-lines))
  (define module-datum `(module day1-mod "lang.rkt"
                          (handle-args ,@src-datums)))
  (datum->syntax #f module-datum))
(provide read-syntax)

(define-macro (day1-module-begin HANDLE-ARGS-EXPR)
  #'(#%module-begin
     ; TODO: DIsplay results
     (display HANDLE-ARGS-EXPR)))
(provide (rename-out [day1-module-begin #%module-begin]))

(define/match (convert s)
  [("one") 1]
  [("two") 2]
  [("three") 3]
  [("four") 4]
  [("five") 5]
  [("six") 6]
  [("seven") 7]
  [("eight") 8]
  [("nine") 9]
  [(n) (string->number n)])

(define pattern #rx"(one|two|three|four|five|six|seven|eight|nine|[0-9])")

(define (handle-args . args)
  (for/sum ([arg (in-list args)]
            #:unless (void? arg))
    (define matches (regexp-match* pattern arg))
    ; this is dumb, but try doing a match from the back, a bit at a time, until we get a digit.
    (define lmatch (let loop ([idx (string-length arg)])
                     (define m (regexp-match pattern (substring arg idx)))
                     (if (or m (< idx 1))
                         ; break, yielding the match or nothing
                         m
                         (loop (sub1 idx)))))
    #; (printf "~a~n" matches)
    #;(when lmatch
        (printf "lmatch ~a, vs ~a~n" lmatch matches))
    (if (not (null? matches))
        (let ([dig1 (convert (first matches))]
              [dig2 (convert (first lmatch))])
          #;(printf "~a~n" (+ (* 10 dig1) dig2))
          (+ (* 10 dig1) dig2))
        0)))
(provide handle-args)
