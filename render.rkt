#lang racket/base
(require racket/list
         racket/match
         racket/string)
(provide string->table
         good-tablestr?)
(define(string->table str)
  (items->table
   
  (map string->items(string-split str ";" #:trim? #f))))
(define(string->items str)
  (map string->item (string-split str "/")))
(define(string->item str)
  (let((a(string-split str)))
    (cons (string->nlist (car a)) (cdr a))))

(define(string->nlist str)
  (match str
     [(regexp #rx"^([0-9]+)-([0-9]+)(.*)" (list _ a b c))(append (range (string->number a) (+ (string->number b) 1)) (string->nlist c))]
     [(regexp #rx"^,(.*)"(list _ a))(string->nlist a)]
     [(regexp #rx"^([0-9]+)(.*)" (list _ a b)) `(,(string->number a) ,@(string->nlist b))]
     [else '()]))
(define(items->table its)
  (define xm(for*/list ([i (in-range 1 7)]
              [j (in-range 1 5)])
    (cons i j)))
  (for/fold([s '()])
           ([i (in-list xm)]
            [j (in-list its)]
             #:unless (null? j))
    (append s
            (for/list ([x (in-list j)])
              (cons (car x) (cons (car i) (cons (cdr i) (cdr x))))))))

(define(good-tablestr? str)
  (andmap good-items? (string-split str ";")))
(define(good-items? str)
  (if(string=? str "")
     #t
     (andmap good-item? (string-split str "/"))))
(define(good-item? str)
  (define a (string-split str))
  (and (= (length a) 3)
      (good-list-str? (car a))))

(define(good-list-str? str)
  (match str
     [(regexp #rx"^([0-9]+)-([0-9]+)(.*)" (list _ a b c))(good-list-str? c)]
     [(regexp #rx"^,(.*)"(list _ a))(good-list-str? a)]
     [(regexp #rx"^([0-9]+)(.*)" (list _ a b)) (good-list-str? b)]
     ["" #t]
     [else #f]))
  
    
