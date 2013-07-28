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


(define my+
  (lambda (a b)
    (cond 
      [(zero? b) a]
      [else (add1 (+ a (sub1 b)))])))

;(+ 190 10)

(define my-
  (lambda (a b)
    (cond 
      [(zero? b) a]
      [else (sub1 (- a (sub1 b)))])))

;(my- 200 10)

(define addtup 
  (lambda (tup)
    (cond 
      [(null? tup) 0]
      [else (+ (car tup) (addtup (cdr tup)))])))

;(addtup '(1 2 3 4 5 6))

(define my* 
  (lambda (a b)
    (cond 
    [(zero? b) 0]
    [else (+ a (my* a (sub1 b)))])))

;;(my* 10 5)
;; This expands to (10 + 10 + 10 + 10 + 10 + 0) due to recursion


(define tup+ 
  (lambda (tup1 tup2) 
    (cond 
      [(and (null? tup1) (null? tup2)) '()] ; Can remove this line, not needed. Why?:)
      [(null? tup1) tup2] ; If tup1 is shorter than tup2, return tup2
      [(null? tup2) tup1]
      [else (cons 
             (+ 
              (car tup1) (car tup2)) 
             (tup+ 
              (cdr tup1) (cdr tup2)))])))

;;(tup+ '(1 2 3) '(2 1 0))
;;(tup+ '(1 1 1) '(3))

(define my>
  (lambda (a b)
    (cond 
      [(zero? a) #f] ; NB Order important ... if a == b
      [(zero? b) #t]
      [else (my> (sub1 a) (sub1 b))])))

;; (my> 10 11)

(define my< 
  (lambda (a b)
    (cond
      [(zero? b) #f]
      [(zero? a) #t]
      [else (my< (sub1 a) (sub1 b))])))


;; (my< 10 11)

(define my= 
  (lambda (a b)
    (not (or (< a b) (> a b)))))

(define my=2
  (lambda (a b)
    (cond
      [(< a b) #f]
      [(> a b) #f]
      [else #t])))

;(my=2 10 11)

(define my-exp
 (lambda (x pow)
   (cond
     [(zero? pow) 1] ; because  x * 1 is x
     [else (* x (my-exp x (sub1 pow)))])))

;; (my-exp 5 3)

(define my-length
  (lambda (lat)
    (cond
      [(null? lat) 0]
      [else (add1 (my-length (cdr lat)))])))

;(my-length '(1 1 2 3 4))

(define my-pick
  (lambda (nth lat)
    (cond 
      [(zero? (sub1 nth)) (car lat)]
      [else (my-pick (sub1 nth) (cdr lat))])))

;; (my-pick 1 '(1 2))

;; Remove the item at the nth place in the list of atoms
(define my-rempick
  (lambda (nth lat) 
    (cond
      [(zero? (sub1 nth)) (cdr lat)]
      [else (cons (car lat) 
                  (my-rempick (sub1 nth) (cdr lat)))])))

;; (my-rempick 2 '(1 2 3 4))

(define no-nums
  (lambda (lat)
    (cond
      [(null? lat) '()]
      [(number? (car lat)) 
         (no-nums (cdr lat))]
      [else 
       (cons (car lat) (no-nums (cdr lat)))])))
  
;; (no-nums '(1 12 3 "a" "b" 1))

(define all-nums
  (lambda (lat)
    (cond
      [(null? lat) '()]
      [(number? (car lat)) (cons (car lat) (all-nums (cdr lat)))]
      [else (all-nums (cdr lat))])))

;; (all-nums '(1 2 "a" 3 4))

(define eqan? 
  (lambda (a b)
    (cond 
      [(and (number? a) (number? b)) (= a b)]
      [else (eq? a b)]
      )
    )
  )
;; (eqan? "a" "a")

(define occur
  (lambda (x lat)
    (cond
      [(null? lat) 0]
      [(eqan? x (car lat)) (add1 (occur x (cdr lat)))]
      [else (occur x (cdr lat))])))

;; (occur 1 '("a" 1 1 1 "b" 1))

(define rember* 
  (lambda (x lat)
    (cond
      [(null? lat) '()] 
      [(atom? (car lat))
         (cond 
           [(eqan? x (car lat)) 
            (rember* x (cdr lat))]
           [else 
            (cons 
              (car lat) 
              (rember* x (cdr lat)))])]
      [else 
         (cons 
          (rember* x (car lat))
          (rember* x (cdr lat)))])))

;; (rember* 1 '(1 (1 2 3) 1 ((1) 1 2)))


(define insertR*
  (lambda (new old the_list)
    (cond 
      [(null? the_list) '()]
      [(atom? (car the_list)) 
       (cond
         [(eqan? old (car the_list)) 
            (cons 
              old 
              (cons 
                new 
                (insertR* new old (cdr the_list))))]
         [else (cons 
                 (car the_list) 
                 (insertR* new old (cdr the_list)))])]
      [else 
        (cons 
          (insertR* new old (car the_list))
          (insertR* new old (cdr the_list)))])))

;; (insertR* "x" "a" '("a" "b" ("a")))