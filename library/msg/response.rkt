#lang racket
(require "parameters.rkt"
         "reply.rkt"
         web-server/servlet
         web-server/servlet-env)

(provide response/msg response/empty)

(define(response/msg msg [from (current-from-user)][to (current-to-user)])
  (response/output (lambda(op)
                     (write-string (msg->string msg from to)op)
                     (void))))

(define(response/empty [from (current-from-user)][to (current-to-user)])
  (response/output (lambda(op)
                     (write-string "<xml></xml>" op)
                     (void))))
  