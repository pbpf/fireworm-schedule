#lang racket
(require racket/runtime-path)

(provide data-path)

(define-runtime-path data-path "./data")
