#lang racket
(provide (all-defined-out))
;
(define current-to-user (make-parameter "toUser"))
(define current-from-user (make-parameter "fromUser"))