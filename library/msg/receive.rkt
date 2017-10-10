#lang racket
(require racket/generic
         xml)

(provide (all-defined-out))

(struct msginfo(to from time type))
;-----------------------
(struct msg msginfo (MsgId))

(struct receivemsg:text msg (Content))
(struct mediamsg msg(MediaId))
(struct receivemsg:image mediamsg (PicUrl))

(struct receivemsg:voice mediamsg (Format Recognition))

(struct receivemsg:video mediamsg (ThumbMediaId))

(struct receivemsg:shortvideo mediamsg (ThumbMediaId))

(struct receivemsg:location msg(Location_X Location_Y Scale Label))

(struct receivemsg:link msg(Title Description Url))

;-----------------------------
(struct event msginfo(Event))

(struct event:unusb/subscribe event())

;-------------------------post-data->xexpr------------------------

(define(delete-topstring lst)
  (filter (lambda(x)(not (string? x)))lst))

(define(post-data->xexpr xmlbytes)
  (cdr(delete-topstring(parameterize([xexpr-drop-empty-attributes #t]
                                                         [collapse-whitespace #t])
                                 (xml->xexpr 
                                             (read-xml/element (open-input-bytes xmlbytes)))))))

(define(get-cdata-item xp tag)
  (cdata-string (get-item xp tag)))

(define(get-item xp tag)
  (cadr (assoc  tag xp)))

(define(get-cdata-item-open xp tag)
  (car(regexp-match #rx"(?<=^<!\\[CDATA\\[).*(?=\\]\\]>$)" (get-cdata-item xp tag))))

(define(get-to xp)
  (get-cdata-item-open xp 'ToUserName))

(define(get-from xp)
  (get-cdata-item-open xp 'FromUserName))

(define(get-msgtype xp)
  (get-cdata-item-open xp 'MsgType))

(define(get-createtime xp)
  (get-item xp 'CreateTime))

;------------------------------------------------------------
(define(receivemsg:get-MsgId xp)
  (get-item xp 'MsgId))

(define(receivemsg:text:get-Content xp)
  (get-cdata-item-open xp 'Content))

(define(receivemsg:media:get-MediaId xp)
  (get-cdata-item-open xp 'MediaId))

(define(receivemsg:voice:get-Format xp)
  (get-cdata-item-open xp 'Format))

(define(receivemsg:voice:get-Recognition xp)
  (get-cdata-item-open xp  'Recognition))

(define(receivemsg:video:get-ThumbMediaId xp)
  (get-cdata-item-open xp 'ThumbMediaId))

(define(receivemsg:shortvideo:get-ThumbMediaId xp)
  (get-cdata-item-open xp 'ThumbMediaId))

(define(receivemsg:location:get-Location_X xp)
  (get-item xp  'Location_X))

(define(receivemsg:location:get-Location_Y xp)
  (get-item xp  'Location_Y))

(define(receivemsg:location:get-Scale xp)
  (get-item xp  'Scale))

(define(receivemsg:location:get-Label xp)
  (get-cdata-item-open xp 'Label))

(define(receivemsg:link:get-Title xp)
  (get-cdata-item-open xp 'Title))

(define(receivemsg:link:get-Description xp)
  (get-cdata-item-open xp 'Description))

(define(receivemsg:link:get-Url xp)
  (get-cdata-item-open xp 'Url))

(define(event:get-Event xp)
  (get-cdata-item-open xp 'Event))
;----------------------------------------------------------------
  