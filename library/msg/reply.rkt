#lang racket/base
(require racket/generic
         "parameters.rkt")

(provide (all-defined-out))


(define-generics replymsg
  (msg->string replymsg from to)
  )

;-----------------------
(struct msg:text  (Content)
  #:methods gen:replymsg
  [(define(msg->string mg from to)
     (format "<xml><ToUserName><![CDATA[~a]]></ToUserName><FromUserName><![CDATA[~a]]></FromUserName><CreateTime>~a</CreateTime><MsgType><![CDATA[text]]></MsgType><Content><![CDATA[~a]]></Content></xml>"
             to from (current-seconds) (msg:text-Content mg)))]
  #:methods gen:custom-write
  [(define(write-proc mg port mode)
     (fprintf port "~a" (msg->string mg (current-from-user)(current-to-user))))]
  
  )
(struct msg:image  (MediaId)
  #:methods gen:replymsg
  [(define(msg->string mg from to)
     (format "<xml><ToUserName><![CDATA[~a]]></ToUserName><FromUserName><![CDATA[~a]]></FromUserName><CreateTime>~a</CreateTime><MsgType><![CDATA[image]]></MsgType><Image><MediaId><![CDATA[~a]]></MediaId></Image></xml>"
             to from (current-seconds)(msg:image-MediaId mg)))]
  #:methods gen:custom-write
  [(define(write-proc mg port mode)
     (fprintf port "~a" (msg->string mg (current-from-user)(current-to-user))))]
  )
(struct msg:news  (ArticleCount Articles)
  #:methods gen:replymsg
  [(define(msg->string mg from to)
     (format
             "<xml><ToUserName><![CDATA[~a]]></ToUserName><FromUserName><![CDATA[~a]]></FromUserName><CreateTime>~a</CreateTime><MsgType><![CDATA[news]]></MsgType><ArticleCount>~a</ArticleCount><Articles>~a</Articles></xml>"
            to from (current-seconds) (msg:news-ArticleCount mg) (apply string-append (map (lambda(x)(format "~a" x)) (msg:news-Articles mg)))))]
  #:methods gen:custom-write
  [(define(write-proc mg port mode)
     (fprintf port "~a" (msg->string mg (current-from-user)(current-to-user))))]
  )
(struct articleitem (Title Description PicUrl Url)
  #:methods gen:custom-write
  [(define(write-proc mg port mode)
     (fprintf port "<item><Title><![CDATA[~a]]></Title><Description><![CDATA[~a]]></Description><PicUrl><![CDATA[~a]]></PicUrl><Url><![CDATA[~a]]></Url></item>"
              (articleitem-Title mg)(articleitem-Description mg) (articleitem-PicUrl mg) (articleitem-Url mg)))]
  )
