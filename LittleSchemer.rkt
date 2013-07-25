#lang racket
(define atom? 
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))


;; List of atoms?
(define lat?
  (lambda (l)
    (cond
      [(null? l) #t]
      [(atom? (car l)) (lat? (cdr l))]
      [else #f])))

(define member? 
  (lambda (item the_list)
    (cond
      [(null? the_list) #f]
      [else (or (eq? item (car the_list)) (member? item (cdr the_list)))])))
  
;(member? "a" '("a" "c"))


;; Remove the first occurence of item from list
(define rember 
  (lambda (item the_list) 
    (cond 
      [(null? the_list) '()]                     ; Always check the null case first
      [(eq? item (car the_list)) (cdr the_list)] ; Found match? Remove it by returning tail of list
      [else (cons (car the_list) (rember item (cdr the_list)))]))) ; Recurr. Build list by keeping head

;(rember "b" '("a" "b" "c"))


(define firsts 
  (lambda (lists)
    (cond 
      [(null? lists) '()]
      [else (cons (car (car lists)) (firsts (cdr lists)))]))) ; Recurr. Build list by keeping the the first element of the first list

;(firsts '(
;         ("ape" "tre")
;         ("katt" "stein")))

 
(define insertR
  (lambda (new old the_list)
    (cond 
      [(null? the_list) '()] ; No items means insert
      [(eq? old (car the_list)) (cons old (cons new (cdr the_list)))] ; Found a match, insert new to the right
      [else (cons (car the_list) (insertR new old (cdr the_list)))] ; No match, build the list with existing items and recurr.
    )))

;(insertR "c" "b" '("a" "b"))

(define insertL
  (lambda (new old the_list)
    (cond
      [(null? the_list) '()] ; The empty list
      [(eq? old (car the_list)) (cons new the_list)]
      [else (cons (car the_list) (insertL new old (cdr the_list)))])))

;(insertL "a" "b" '("b" "c" "d"))


(define subst
  (lambda (new old the_list)
    (cond 
      [(null? the_list) '()] ; No items
      [(eq? old (car the_list)) (cons new (cdr the_list))] ; Found a match, replace
      [else (cons (car the_list) (subst new old (cdr the_list)))] ; No match, build the list with existing items and recurr.
    )))

;(subst "b" "a" '("a" "c" "a"))

(define subst2
  (lambda (new old1 old2 the_list)
    (cond
      [(null? the_list) '()]
      [(or 
        (eq? old1 (car the_list)) 
        (eq? old2 (car the_list))) 
       (cons new (cdr the_list))]
      [else (cons (car the_list) (subst2 new old1 old2 (cdr the_list)))])))


(define multirember
  (lambda (remove the_list)
    (cond
      [(null? the_list) '()]
      [(eq? remove (car the_list)) (multirember remove (cdr the_list))]
      [else (cons (car the_list) (multirember remove (cdr the_list)))])))

;(multirember "a" '("a" "b" "c" "a" "a"))

(define multiinsertR
  (lambda (new old the_list)
    (cond
      [(null? the_list) '()]
      [(eq? old (car the_list)) (cons old (cons new (multiinsertR new old (cdr the_list))))]
      [else (cons (car the_list) (multiinsertR new old (cdr the_list)))])))

;(multiinsertR "x" "a" '("a" "b" "a" "c" "d" "a"))

(define multiinsertL
  (lambda (new old the_list)
    (cond
      [(null? the_list) '()]
      [(eq? old (car the_list)) (cons new (cons old (multiinsertL new old (cdr the_list))))]
      [else (cons (car the_list) (multiinsertL new old (cdr the_list)))])))

;(multiinsertL "x" "a" '("a" "b" "a" "c" "d" "a"))


(define multisubst
  (lambda (new old the_list)
    (cond 
      [(null? the_list) '()]
      [(eq? old (car the_list)) (cons new (multisubst new old (cdr the_list)))]
      [else (cons (car the_list) (multisubst new old (cdr the_list)))]
      )
    )
  )

;(multisubst "x" "a" '("b" "a" "a" "c" "a"))




