#lang racket
(require advent-of-code)
(require data/interval-map)

; use a hash for each map, but hash-ref should return the key itself when the value is missing.
; maintain a list of interval-maps, one from each map to the next.
; this should be enough for part 1
; seeds are not a map, just a list
(struct almanac (seeds maps) #:transparent)

; if we use data/interval-map, the keys can be the source ranges
; however we need to associate the destination with a range, and then translate from the offset
; i.e. dst src len
; then looking up src+i where i < len should eventually result in dst+i
; if interval-map-ref/bounds is used. this may work

; need a better structure than a direct per-key hash map since the numbers are so big.

(define (read-seeds)
  (define line (read-line))
  (match-define
    (regexp #rx"seeds: (.+)"
            (list _ seed-str))
    line)
  (map string->number (string-split seed-str)))

(define (read-range)
  (let ([c (peek-char)])
    (if (or (eq? #\newline c) (eof-object? c))
        #f
        (map string->number (string-split (string-trim (read-line)))))))

(define (read-map-no-eof)
  ; read empty line
  (read-line)
  ; read map name which we don't care about
  (read-line)
  (let loop ([map (make-interval-map)])
    (define range (read-range))
    (if range
        (begin
          #;(printf "Range is ~v~n" range)
          ; convert range to hash entries
          (match-let ([(list dst-start src-start len) range])
            (interval-map-set! map src-start (+ src-start len) dst-start)
            (loop map)))
        map)))

(define (read-map)
  (let ([c (peek-char)])
    (if (eof-object? c)
        #f
        (read-map-no-eof))))

(define (read-almanac)
  ; first line is seeds
  (almanac
   (read-seeds)
   (let loop ([maps null])
     (let ([m (read-map)])
       (if m
           (loop (cons m maps))
           (reverse maps))))))

(define (min-location almanac)
  (for/fold ([min-loc +inf.0]
             #:result (exact-truncate min-loc))
            ([seed (in-list (almanac-seeds almanac))])  
    (min min-loc
         ; translate from seed to location by going throught the maps
         (for/fold ([item seed])
                   ([map (in-list (almanac-maps almanac))])
           (define-values (start end-ex val) (interval-map-ref/bounds map item item))
           #;(printf "Lookup of ~v resulted in ~v~n" item (list start end-ex val))
           (if start
               (+ val (- item start))
               val)))))

(define (run)
  (define almanac (read-almanac))
  #;(printf "Almanac ~a~n" almanac)
  (values (min-location almanac) +nan.0))

(module+ main
  (parameterize ([current-input-port (open-aoc-input (find-session) 2023 5 #:cache #t)])
    (define-values (p1 p2) (run))
    (printf "Part 1: ~v~n" p1)))

(module+ test
  (require rackunit)
  (define-values (p1 p2)
    (with-input-from-string "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4" run))
  (check-equal? p1 35))