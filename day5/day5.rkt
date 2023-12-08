#lang racket
(require advent-of-code)

; use a hash for each map, but hash-ref should return the key itself when the value is missing.
; maintain a list of interval-maps, one from each map to the next.
; this should be enough for part 1
; seeds are not a map, just a list
(struct almanac (seeds maps) #:prefab)

; we will need our own interval-map replacement that is much faster.
; for whatever reason, interval map is slow.
; end exclusive
(struct range (start end dest) #:prefab)


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
  (let loop ([map null])
    (define r (read-range))
    (if r
        (begin
          #;(printf "Range is ~v~n" range)
          ; convert range to entries
          (match-let ([(list dst-start src-start len) r])
            (loop (cons (range src-start (+ src-start len) dst-start) map))))
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

(define (lookup alm-map key)
  (let loop ([l alm-map])
    (if (empty? l)
        (values #f #f key)
        (let ([r (first l)])
          #;(printf "Range is ~v~n, key is ~v" r key)
          (if (and (>= key (range-start r))
                   (< key (range-end r)))
              (values (range-start r) (range-end r) (range-dest r))
              (loop (rest l)))))))

(define (min-location almanac)
  (for/fold ([min-loc +inf.0]
             #:result (exact-truncate min-loc))
            ([seed (in-list (almanac-seeds almanac))])
    #;(printf "Seed ~v~n" seed)
    (min min-loc
         ; translate from seed to location by going throught the maps
         (for/fold ([item seed])
                   ([map (in-list (almanac-maps almanac))])
           (define-values (start end-ex val) (lookup map item))
           #;(printf "  Lookup of ~v resulted in ~v~n" item (list start end-ex val))
           (if start
               (+ val (- item start))
               val)))))

(define (min-location-seed-range almanac seed-range)
  (printf "Running on range ~v, ~v items~n" seed-range (- (range-end seed-range) (range-start seed-range)))
  
  (time (for/fold ([min-loc +inf.0]
             #:result (exact-truncate min-loc))
            ([seed (in-range (range-start seed-range) (range-end seed-range))])
    #;(printf "Seed ~v~n" seed)
    (min min-loc
         ; translate from seed to location by going throught the maps
         (for/fold ([item seed])
                   ([map (in-list (almanac-maps almanac))])
           (define-values (start end-ex val) (lookup map item))
           #;(printf "  Lookup of ~v resulted in ~v~n" item (list start end-ex val))
           (if start
               (+ val (- item start))
               val))))))

(define (run-lookup-in-place almanac seed-range)
  (define p
    (place ch
           (match-define (list alm range) (place-channel-get ch))
           (printf "Place got ~v~n" range)
           (place-channel-put ch (min-location-seed-range alm range))))
  (place-channel-put p (list almanac seed-range))
  p)

(define (min-location-2 almanac)
  ; convert seeds to another map
  (define seed-map 
    (let loop ([l (almanac-seeds almanac)]
               [sm null])
      (if (empty? l)
          sm
          (loop (drop l 2) (cons (range (first l) (+ (first l) (second l)) #t) sm)))))

  (printf "Seed map is ~v~n" seed-map)
  (define channels (for/list ([seed-range (in-list seed-map)])
                     (run-lookup-in-place almanac seed-range)))
  ; [seed-range (in-dict-keys seed-map)]
  ; ok, generally proceed as before, except the seed iterator should treat seeds to check as the bounds of all the seed-map keys
  ; get answers from all places, and take their minimum
  (argmin identity (map place-channel-get channels))
  #;
  (for/fold ([min-loc +inf.0]
             #:result (exact-truncate min-loc))
            ([seed-range (in-list seed-map)])
    (min min-loc
         (min-location-seed-range almanac seed-range))))

(define (run)
  (define almanac (read-almanac))
  (printf "Almanac ~a~n" almanac)
  (values (min-location almanac) (min-location-2 almanac)))

(module+ main
  (parameterize ([current-input-port (open-aoc-input (find-session) 2023 5 #:cache #t)])
      (define-values (p1 p2) (run))
      (printf "Part 1: ~v~n" p1)
      (printf "Part 2: ~v~n" p2)))

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
  (check-equal? p1 35)
  (check-equal? p2 46))