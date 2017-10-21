#lang racket/base
(require  net/url
          json)

(provide  url-get-access_token)
          

(define(url-get-access_token appid appSecret)
  (define url (string->url(format "https://api.weixin.qq.com/cgi-bin/token?grant_type=client_credential&appid=~a&secret=~a" appid appSecret)))
  (read-json (get-pure-port url)))



