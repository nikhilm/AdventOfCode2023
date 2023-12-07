#lang racket

(require advent-of-code)

(define (run)
  (define input (for/vector ([b (in-input-port-chars (current-input-port))]
                             #:unless (eq? #\newline b))
                  b))
  (define stride (sqrt (vector-length input)))
  (define -stride (- stride))

  (define (relevant-symbol? c)
    (and (not (char-numeric? c))
         (not (eq? #\. c))))

  (define (has-adjacent? pos fn)
    (for*/first ([delta (in-list (list (- -stride 1) -stride (+ -stride 1)
                                                -1           +1
                                      (-  stride 1)  stride (+  stride 1)))]
                 [idx (in-value (+ pos delta))]
                #:when (and (>= idx 0)
                            (< idx (vector-length input))
                            (fn (vector-ref input idx))))
      idx))

  (define (char->num c)
    (- (char->integer c) (char->integer #\0)))

  ; when a digit is encountered, start treating it as a number run
  ; then only add it to the total if, while reading it, any digit was adjacent to a symbol.
  ; watch out for 2 numbers at a stride boundary. don't treat them as one number.
  (define p1 (for/fold ([cur-num 0]
             [adj-sym? #f]
             [total 0]
             ; this handles getting only one value + it handles an ending run of digits.
             #:result (+ total cur-num))
            ([(c idx) (in-indexed input)])
    (if (char-numeric? c)
        (values (+ (* 10 cur-num) (char->num c))
                ; if this run encountered a sym anywhere, preserve that
                (or adj-sym? (has-adjacent? idx relevant-symbol?))
                total)
        ; run finished
        (begin
          #; (printf "end run ~a. considered ~a~n" cur-num adj-sym?)
          (values 0 #f (if adj-sym? (+ total cur-num) total))))))
  (printf "part 1? ~a~n" p1)

  (define (gear? c)
    (eq? #\* c))

  ;iterate over the vec and as we track numbers, see if a number is next to a gear
  ; if it is, track the gear idx for the list
  (define nums-and-gears (for/fold ([cur-num 0]
             [adj-gear? #f]
             [nums-and-gears null]
             ; TODO: Handle end run
             #:result (if adj-gear? (cons `(,adj-gear? . ,cur-num) nums-and-gears) nums-and-gears))
            ([(c idx) (in-indexed input)])
    (if (char-numeric? c)
        (values (+ (* 10 cur-num) (char->num c))
                (or adj-gear? (has-adjacent? idx gear?))
                nums-and-gears)
        (values 0 #f (if adj-gear? (cons `(,adj-gear? . ,cur-num) nums-and-gears) nums-and-gears)))))
  #;(printf "h ~a~n" nums-and-gears)
  (define ratios
    (for/fold ([gear-to-nums (hash)])
              ([g-n (in-list nums-and-gears)])
      #;(printf "hmm ~a ~a ~a ~n" gear-to-nums g-n (list (cdr g-n)))
      (hash-update gear-to-nums
                   (car g-n)
                   (lambda (existing)
                     (cons (cdr g-n) existing))
                   (lambda () null))))
  #;(printf "ratios ~a~n" ratios)
  (define pt2 (for/sum ([nums (in-hash-values ratios)]
            #:when (= 2 (length nums)))
    (* (first nums) (second nums))))
  (printf "part 2? ~a~n" pt2))

(module+ main
  (parameterize ([current-input-port (open-aoc-input (find-session) 2023 3 #:cache #t)])
    (run)))

(module+ test
  (with-input-from-string
"467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."
    run))