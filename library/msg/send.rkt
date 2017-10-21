#lang racket/base
(require racket/generic
         json)

(provide (all-defined-out))


(define-generics sendmsg
  (msg->string  sendmsg to)
  )
(struct sendmsg:text (text)
   #:methods gen:sendmsg
  [(define(msg->string t to)
     (jsexpr->string
      (hash 'touser to
            'msgtype "text"
            'text (hash  'content (sendmsg:text-text t)))))])

(struct sendmsg:pict(pictid)
   #:methods gen:sendmsg
  [(define(msg->string t to)
     (jsexpr->string
      (hash 'touser to
            'msgtype "image"
            'image (hash  'media_id (sendmsg:pict-pictid t)))))])
            
            
;Y3EB-T5t5thPiwhXr6X-cr6AME--MntgudQsu4gfKWlgkVgmoTDTVc4UcN-_V_dHNtL7pvNo7tZcrCXz-TfKVBP2h38timWIGH4aZ4OKjLkPAQgAHAZKA
