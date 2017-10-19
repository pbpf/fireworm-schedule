#lang racket
(require db
         racket/date
         pict
          web-server/servlet
         web-server/servlet-env
         file/sha1
         xml)

(provide (all-defined-out))
;class-schedule
;sql structure
;server as racket? python? c++? java? js? sql?
(define(init-sql! cnn)
  (query-exec cnn "CREATE TABLE classsd (week INT NOT NULL, weekday INT NOT NULL, rank INT NOT NULL, user, room CHAR, name CHAR, lastremind INT, PRIMARY KEY (week, weekday, rank, user))")
  (query-exec cnn "CREATE TABLE users (name CHAR NOT NULL PRIMARY KEY, enable INT)")
  (query-exec cnn "CREATE TABLE timerange (rank INT PRIMARY KEY, start INT, end INT)")
  )

(define(range* a b)
  (range a (add1 b)))


(define(insert-sd cnn tlst user)
  (for ([i (in-list tlst)])
    (insert-sd-weeks  cnn i user)))
(define(insert-sd-weeks cnn lst user)
  (for ([i (in-list (car lst))])
    (apply query-exec cnn "insert into classsd (week,user,weekday,rank,room,name) values($1,$2,$3,$4,$5,$6)"
           i user (cdr lst) )))

(define time-range '( "08:00-09:40" "10:20-12:00" "14:30-17:00" "19:40-22:15"))

(define(time-str? str)
  (regexp-match-exact? #rx"([0-1][0-9]|2[0-3]|[0-9]):[0-5][0-9]" str))
(define(time-range-str? str)
  (regexp-match-exact? #rx"([0-1][0-9]|2[0-3]|[0-9]):[0-5][0-9]-([0-1][0-9]|2[0-3]|[0-9]):[0-5][0-9]" str))
(define(insert-timerg cnn lst)
  (for ([i (in-range 1 5)]
        [j (in-list lst)])
    (insert-timerg-str cnn i j)))
(define(insert-timerg-str cnn  i str)
  (if(time-range-str? str)
     (apply query-exec cnn "insert into timerange (rank,start,end) values($1,$2,$3)"
            i (map time->sec (string-split str "-")))
     (error 'insert-timerg-str "~s is not a time-range string!" str)))
     
;no-check
(define (time->sec str)
     (let([s(string-split str ":")])
       (+ (* (string->number(car s)) 3600)
          (* (string->number (cadr s)) 60))))

(define week-point-monday (vector "2017-10-9" 6 28800))

;utc
(define(ymd->sec ymd #:time-zone-offset [gmt-offset (current-gmt-offset)])
  (find-seconds 0 0 0 (list-ref ymd 2) (list-ref ymd 1) (list-ref ymd 0) #f))
(define(which-week-is-sec sec point-sec point-week)
  (let-values([(a b)(quotient/remainder (- sec point-sec) (* 3600 24 7))])
    (if(>= sec point-sec)
       (values (+ point-week a) (+ (quotient b (* 3600 24)) 1 ))
       (if(zero? b)
          (values (+ point-week a) 1)
          (values (+ point-week   a -1) (+ 8 (quotient b (* 3600 24))))))))
  #|
 (if(>= sec point-sec)
     (quotient (- sec point-sec) (* 3600 24 7))
     (floor (/ (- sec point-sec) (* 3600 24 7)))))
|#

(define(which-week-is-ymd ymd point-ymd point-week)
  (which-week-is-sec (ymd->sec ymd) (ymd->sec point-ymd) point-week))
;api

(define current-gmt-offset (make-parameter (* 8 3600)))

;(define(date->sec* date)
 ; (find-seconds
(define(date->sec dt)
  (- (date->seconds dt #f)
     (date-time-zone-offset dt)
     ))
(define(date->utc-date dat)
  (seconds->date (date->sec dat) #f))

(define(utc-date->gmt-date gdt  #:time-zone-offset [gmt-offset (current-gmt-offset)])
  (struct-copy date (seconds->date (+ (date->sec gdt) gmt-offset) #f)
    [time-zone-offset gmt-offset]))
(define(date->gmtdate dat #:time-zone-offset [gmt-offset (current-gmt-offset)])
  (utc-date->gmt-date (date->utc-date dat) #:time-zone-offset gmt-offset))
(define(today-ymd)
  (date->ymd (current-date)))
(define(date->ymd d #:time-zone-offset [gmt-offset (current-gmt-offset)])
  (map (lambda(x)(x (date->gmtdate d #:time-zone-offset gmt-offset))) (list date-year date-month date-day)))
;ymd schedule
#|
(define(current-point a b)
  week-point-monday)
|#
(define(ymdstr->ymd ymdstr)
  (map string->number(string-split ymdstr "-")))
(define(which-week-is cnn ymd user)
  ;(displayln user)
  (let((a week-point-monday))
    (which-week-is-ymd ymd (ymdstr->ymd(vector-ref a 0)) (vector-ref a 1))))
;;;;;;;;;api----------------
(define(day-ymd->schedule cnn ymd user)
  (let-values([(w d)(which-week-is cnn ymd user)])
    (query-rows cnn "select rank,room,name from classsd where user=$1 and week=$2 and weekday=$3" user w d)))
;----api------
(define(ymd-rank-class cnn ymd rank user)
  (let-values([(w d)(which-week-is cnn ymd user)])
    (query-rows cnn "select room,name,lastremind from classsd where user=$1 and week=$2 and weekday=$3 and rank=$4" user w d rank)))
(define(week->schedule cnn week user)
  (query-rows cnn "select weekday,rank,room,name from classsd where user=$1 and week=$2" user week))
(define(user-schedule cnn user)
  (query-rows cnn "select week,weekday,rank,room,name from classsd where user=$1 order by week*100+weekday*10+rank" user))

;;;render
(define(class-render rank room name rank-vec)
  (format "时间: ~a 地点: ~a 科目: ~a" (vector-ref rank-vec (sub1 rank)) room name))
(define rank-vec-t #("1,2" "3,4" "5,6,7" "8,9,10"))

(define weekvec (vector "一" "二" "三" "四" "五" "六" "日"))
(define (index->weekday i)
  (vector-ref weekvec (sub1 i)))
;;;
(define(table-list cnn user)
  (apply append
  (cons (list "星期/节次" "1,2" "3,4" "5,6,7" "8,9,10")
  (for/list([i (in-range 1 8)])
    `( ,(index->weekday i),@(for/list
            ([j (in-range 1 5)])
    (class->str cnn i j user)))))))
(define(table-list-week cnn user week)
  (apply append
  (cons (list "星期/节次" "1,2" "3,4" "5,6,7" "8,9,10")
  (for/list([i (in-range 1 8)])
    `( ,(index->weekday i),@(for/list
            ([j (in-range 1 5)])
    (weekclass->str cnn i j  week user)))))))
;api
(define(user-schedule-pict cnn user)
  (table 5
         (map (λ (x) (text x))
              (table-list cnn user))
         lc-superimpose
         cc-superimpose
         20
         10))
;;;;;;;;;;;;;api
(define(user-schedule-week-pict cnn user week)
  (table 5
         (map (λ (x) (text x))
              (table-list-week cnn user week))
         lc-superimpose
         cc-superimpose
         20
         10))
(define(weekclass->str cnn week-day rank week user)
  (string-join 
  (map (lambda(x)(format "~a ~a" (vector-ref x 0) (vector-ref x 1)))
 (query-rows cnn "select room,name from classsd where user=$1 and weekday=$2 and rank=$3  and week=$4" user week-day rank week))
  ))
     

(define(class->str cnn week-day rank user)
  (define lst (remove-duplicates(query-rows cnn "select room,name from classsd where user=$1 and weekday=$2 and rank=$3 order by week" user week-day rank)))
  (string-join(for/list([i (in-list lst)])
    (format "~a ~a(~a周)" (vector-ref i 0) (vector-ref i 1)
            (list->tbstr
     (query-list cnn "select week from classsd where user=$1 and weekday=$2 and rank=$3 and room=$4 and name=$5 order by week"
                 user week-day rank (vector-ref i 0) (vector-ref i 1))
     ))) ";\n"))
(define(list->tbstr lst)
  (list-pair-mix->str
  (for/fold([t '()])
           ([i (in-list lst)])
    (update-t t i))))
(define(list-pair-mix->str lst)
  (string-join(for/list ([i (in-list lst)])
    (if(pair? i)
       (format "~a-~a" (car i)(cdr i))
       (format "~a" i)))
              ","))
(define(update-t t i)
  (if(null? t)
     (list i)
     (if(pair? (car t))
        (if(match-pair? (car t) i)
           (cons (pair-add1 (car t)) (cdr t))
           (cons (car t)(update-t (cdr t) i)))
        (if(= (+ 1 (car t)) i)
           (cons (cons (car t) i) (cdr t))
           (cons (car t) (update-t (cdr t) i))))))
(define(match-pair? a b)
  (= (+ 1 (cdr a)) b))
(define(pair-add1 a)
  (cons (car a) (+ 1 (cdr a))))

(define(rrn->str i)
  (class-render (vector-ref i 0) (vector-ref i 1) (vector-ref i 2) (list->vector time-range)))
(define(find-next-rank cnn b)
  (for/first ([(r s e)(in-query cnn "select rank,start,end from timerange order by rank")]
              #:when  (<= b s))
    r))
(define(find-this-rank cnn b)
  (for/first ([(r s e)(in-query cnn "select rank,start,end from timerange order by rank")]
              #:when  (<= s b e))
    r))
;-----------------------api----base----------------------------------------------------------------------------------------

(define(ymd-schedule cnn ymd user)
  (string-join 
  (for/list ([i(in-list (day-ymd->schedule cnn ymd user))])
    (rrn->str i))
  "\n"))

(define(today-schedule cnn user)
  (ymd-schedule cnn (today-ymd) user))
(define(nextday-schedule cnn user)
  (ymd-schedule cnn (date->ymd (seconds->date (+ (current-seconds) (* 24 3600)))) user))

(define(gmt-today-sec sec #:time-zone-offset [offsec (current-gmt-offset)])
  (define a (date->gmtdate (seconds->date sec)))
  (+ (date-second a) (* 60 (date-minute a))(* 3600 (date-hour a))))
(define(next-class cnn user #:sec [sec (current-seconds)] #:time-zone-offset [offsec (current-gmt-offset)])
 ; (define a (today-ymd)) ymd
  (define a (date->ymd (seconds->date sec) #:time-zone-offset offsec))
 ; (define b (-(current-seconds)(ymd->sec a)))
  (define b (gmt-today-sec sec #:time-zone-offset offsec))
  (define c (find-next-rank cnn b))
  (if c
      (for/first ([i (in-range c 5)]
                  #:unless (null? (ymd-rank-class cnn a i user)))
       ; (displayln (format "~a ~a" a i))
         (define x (car(ymd-rank-class cnn a i user)))
        (class-render i  (vector-ref x 0) (vector-ref x 1) (list->vector time-range)))
      #F))
(define(this-class cnn user #:sec [sec (current-seconds)] #:time-zone-offset [offsec (current-gmt-offset)])
 ; (define a (today-ymd)) ymd
  (define a (date->ymd (seconds->date sec) #:time-zone-offset offsec))
 ; (define b (-(current-seconds)(ymd->sec a)))
  (define b (gmt-today-sec sec #:time-zone-offset offsec))
  (define c (find-this-rank cnn b))
  (if c
      (let((class(ymd-rank-class cnn a c user)))
        (if(null? class)
           #f
        (class-render c (vector-ref (car class) 0) (vector-ref (car class) 1) (list->vector time-range))))
      #F))

(define(schedule cnn user)
  (user-schedule-pict cnn user))
(define(which-week-is-today cnn user)
  (define-values(a b)(which-week-is cnn (today-ymd) user))
  a)
(define(this-week-schedule cnn user)
  (user-schedule-week-pict cnn user (which-week-is-today cnn user)))
(define(next-week-schedule cnn user)
  (user-schedule-week-pict cnn user (+ 1(which-week-is-today cnn user))))
                
