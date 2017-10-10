#lang racket/base
(require file/sha1)

(provide  check_signature_hashtable check_signature)


(define (check_signature timestamp nonce token signature)
   (define str (apply bytes-append (sort (list timestamp nonce token)bytes<?)))
  ;(displayln str)
   ;(displayln (sha1 (open-input-bytes str)))
  
     (bytes=? (string->bytes/utf-8(sha1 (open-input-bytes str))) signature))

(define (check_signature_hashtable token table)
  (if(and (hash-has-key? table #"timestamp")
          (hash-has-key? table #"nonce")
          (hash-has-key? table #"signature")
          (hash-has-key? table #"echostr"))
  (check_signature (hash-ref table  #"timestamp") (hash-ref table  #"nonce") token (hash-ref table  #"signature"))
  #f))
;
(module+ test
  (check_signature_hashtable
   #"kajsiui125"
    #hash((#"nonce" . #"3967055129") (#"timestamp" . #"1507545797") (#"signature" . #"938de9b1f4d98123e266b6201a171e3eeac39a13") (#"echostr" . #"4309187672871261125"))
    ))