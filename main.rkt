#lang racket/base
(require db/base
         db/sqlite3
         racket/date
         racket/string
         web-server/servlet
         racket/match
         xml
         "schedule.rkt"
         "configure.rkt"
         "./library/msg/main.rkt"
         "./library/access/signature.rkt"
         "./render.rkt")
(provide start)

(define cnn ;(sqlite3-connect #:database data-path))
  (virtual-connection (connection-pool (lambda () (sqlite3-connect #:database data-path)))))
#|
(define (create-schedule time cmd)
  #f)
|#
(define(notremind-yet? nc)
  (if(sql-null? (vector-ref nc 2))
     #t
     (> (abs (-  (vector-ref nc 2) (current-seconds)) 180))))
(define(get-remind-time cnn r user)
  (query-value cnn "select start from timerange where user=$1" user))
(define(today-sec)
  (define a (current-date))
  (+ (date-second a) (* 60 (date-minute a)) (* 3600 (date-hour a))))
(define(can-remind-now? t)
  (define a (today-sec))
  (<= (abs (- a t)) 60))
#|
(define(remind-me cnn user)
  (define nc (next-class-base cnn user))
  (when (and nc (notremind-yet? (cadr nc)))
    (define t(get-remind-time cnn (car nc) user))
    (if(can-remind-now? t)
       (remind-user-class user nc t)
       (create-schedule t (get-current-remind-cmd cnn user)))))
|#


;(define ss (open-output-file "log.txt" #:exists 'append))

(define(removejh str)
  (define x(regexp-match  #rx"^(.*?)(。|[.]).*" str))
  (if x (cadr x)
      str))

(define(match-cmd text)
  (case text
    [("下节课" "xjk" "下节" "xj" "现在" "xz") 'xjk]
    [("这节课" "zjk" "这节" "zj") 'zjk]
    [("今天" "jt" "今天的课" "jtdk") 'jtdk]
    [("明天" "mt" "明天的课" "mtdk") 'mtdk]
    [("这周" "zz" "本周" "bz" "这周的课" "zzdk" "本周的课" "bzdk") 'zzdk]
    [("下周" "xz" "下周的课" "xzdk") 'xzdk]
    [("课表" "kb" "学期" "xq"  "学期课表" "xqkb") 'xqkb]
    [("日期" "rq") 'rq]
    [else 'unmatch]))
(define cmdlist
  '( ("下节课" "xjk" "下节" "xj" "现在" "xz")
     ("这节课" "zjk" "这节" "zj")
     ("今天" "jt" "今天的课" "jtdk")
     ("明天" "mt" "明天的课" "mtdk")
     ("这周" "zz" "本周" "bz" "这周的课" "zzdk" "本周的课" "bzdk")
     ("下周" "xz" "下周的课" "xzdk")
     ("课表" "kb" "学期" "xq"  "学期课表" "xqkb")))
(define cmd-str
  (string-join 
  (for/list ([i (in-range 1 100)]
             [j (in-list (map (lambda(x)(string-join x ",")) cmdlist))])
    (format "~a:~a;" i j)) "\n"))
(define(restr* x)
  (if (and x (not (string=? x ""))) x
      "没课"))

(define(user-has-no-kb? user)
  (zero?(query-value cnn "select count(*) from users where name=$1 and enable=1" user)))

(define helpput-str "使用如下格式上传课表:\nput:课程1;课程2;课程3;...课程i;...;课程n\n课程i: 6-17 102-303 数学\n课程i: 6-17,19 102-303 数学\18-21 101-303 英语\n课程i: 没有课时保留分号\n更多细节请咨询管理员")
                  

(define(rp-cmd cmd  #:user [user (current-to-user)])
   ;(write user)
 ; (newline)
  (if(eq? cmd 'unmatch)
     (response/msg (msg:text (format "使用如下指令:\n~a\n~a"cmd-str helpput-str)))
     (if (user-has-no-kb? user)
         (response/msg (msg:text (format "您还没有设置课表!\n~a" helpput-str)))
         (case cmd
           [(rq)(define x (date->gmtdate (current-date)))
                (define t (today-ymd))
                (define-values(a b)(which-week-is cnn t user))
                (response/msg (msg:text (format "~a 第~a周星期~a"(parameterize([date-display-format 'iso-8601])
                                                              (date->string x #t)) a b)))]
           [(xjk)(response/msg (msg:text (restr*(next-class cnn user))))]
           [(zjk)(response/msg (msg:text (restr*(this-class cnn user))))]
           [(jtdk)(response/msg (msg:text (restr*(today-schedule cnn user))))]
           [(mtdk)(response/msg (msg:text (restr*(nextday-schedule cnn user))))]
           [else (response/msg (msg:text "功能实现中..."))]))))
           
(define (replay-post req)
  ;(displayln req)
  (define xp (post-data->xexpr (request-post-data/raw req)))
  (define msgtype (get-msgtype xp))
  (parameterize([current-to-user (get-from xp)]
                [current-from-user(get-to xp)])
    (case msgtype
      [("text");(displayln (receivemsg:text:get-Content xp)) (flush-output)
       (define txt (receivemsg:text:get-Content xp))
       (match txt
         [(regexp #rx"put:(.*)" (list _ a)) (cond
                                              [(good-tablestr? a)
                                               ;(displayln (current-to-user))
                                               (call/cc (lambda(return)
                                               (call-with-transaction cnn
                                                                      (lambda()
                                                                        ;(displayln (query-value cnn "select count(*) from classsd where user=$1"(current-to-user)))
                                                                        (query-exec cnn "delete from classsd where user=$1" (current-to-user))
                                                                        ;(displayln (query-value cnn "select count(*) from classsd where user=$1"(current-to-user)))
                                                                        (with-handlers([exn:fail:sql? (lambda(e)(return  (response/msg (msg:text "课表有冲突" ))))])
                                                                        (insert-sd cnn (string->table a) (current-to-user)))
                                                                        ;(displayln (query-value cnn "select count(*) from classsd whereuser=$1"(current-to-user)))
                                                                        (query-exec cnn "insert or replace into users(name,enable) values($1,$2)" (current-to-user) 1)))
                                                (return (response/msg (msg:text "课表已上传/更新" )))))]
                                              [else (response/msg (msg:text "课表格式有误!" ))])]
         [else
                (rp-cmd (match-cmd  txt)) ])]
      [("voice");(displayln (removejh(receivemsg:voice:get-Recognition xp)))(flush-output)
                 (rp-cmd (match-cmd  (removejh(receivemsg:voice:get-Recognition xp))))]
      [("event")(response-event xp)]
      [else  (response/msg (msg:text "不能识别的消息" ))])
      ))

(define(list->hash lst)
  (for/hash([it (in-list lst)]
            #:when (binding:form? it))
    (values (binding-id it) (binding:form-value it))))
(define(start req)
 ; (displayln req)
  (case (request-method req)
    [(#"GET");(displayln req)
             (define t(list->hash(request-bindings/raw req)))
             ;(printf "table: ~s\n" t)
             (if (check_signature_hashtable #"kajsiui125" t)
               (response/output	(lambda(out)(display (hash-ref t #"echostr") out)))
               (response/output	(lambda(out)(display "error！" out))))]
    [(#"POST")(replay-post req)]
    [else (response/msg (msg:text "不能识别的消息" ))]))

(define(response-event xp)
  (define ev (event:get-Event xp))
  (case ev
    [("subscribe")(response/msg (msg:text "您好,欢迎加入萤火虫计划!"))]
    [("unsubscribe")(response/empty)]
    [else (response/msg (msg:text "不能识别的消息:\n~a" (xexpr->string (cons 'xml xp))))]))


