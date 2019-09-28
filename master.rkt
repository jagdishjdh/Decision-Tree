#lang racket

(require "decision_functions.rkt")
(require 2htdp/batch-io)

;input dataset
(provide toytrain)
(define toytrain "../data/toy_train.csv")

(provide titanictrain)
(define titanictrain "../data/titanic_train.csv")

(provide mushroomtrain)
(define mushroomtrain "../data/mushrooms_train.csv")

;output tree (dot file)
(provide toyout)
(define toyout "../output/toy-decision-tree.dot")

;reading input datasets
;read the csv file myfile as a list of strings
;with each line of the original file as an element of the list
;further split each line at commas
;so then we have a list of list of strings

(provide toy-raw)
(define toy-raw (cdr (read-csv-file toytrain)))

(provide titanic-raw)
(define titanic-raw (map cddr (cdr (read-csv-file titanictrain))))

(provide mushroom-raw)
(define mushroom-raw (cdr (read-csv-file mushroomtrain)))

;function to convert data to internal numerical format
;(features . result)
(provide format)
(define (format data) (cons (map string->number (cdr data)) (string->number (car data))))

;list of (features . result)
(provide toy)
(define toy (map format toy-raw))
;
(provide titanic)
(define titanic (map format titanic-raw))

(provide mushroom)
(define mushroom (map format mushroom-raw))

;============================================================================================================
;============================================================================================================
;============================================================================================================

;get fraction of result fields that are 1
;used to find probability value at leaf
(provide get-leaf-prob)
(define (get-leaf-prob data)
  (/ (foldr (lambda (x y) (+ (cdr x) y)) 0 data) (length data)))

;get entropy of dataset
(provide get-entropy)
(define (get-entropy data)
  (define p (get-leaf-prob data))
  (define q (- 1 p)) ;;; p + q = 1
  (cond [(or (= p 0) (= q 0)) 0]
        [else (+ (* p (log p 0.5)) (* q (log q 0.5)))]))

;find the difference in entropy achieved
;by applying a decision function f to the data
(provide entropy-diff)
(define (entropy-diff f data)
  (let ([initial (get-entropy data)]
        [final (/ (foldr + 0 (map (lambda (x) (* (length x) (get-entropy x))) (f data)))
                  (length data))])
    (- initial final)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (mymax l f) ;; f is function to determine larger member
  (define (helper l largest)
    (cond [(null? l) largest]
          [(f (car l) largest) (helper (cdr l) (car l))]
          [else (helper (cdr l) largest)]))
  (helper (cdr l) (car l)))

(define (mypack l) ;;; list of cons(a b) pack wrt a
  (match l
    [(cons (cons a b) '()) (list (cons a (list b)))]
    [(cons (cons a b) rest) (let ([done (mypack rest)])
                              (match done
                                [(cons (cons c l) other) (if (= c a) (cons (cons c (cons b l)) other)
                                                             (cons (cons a (list b)) done))]))]))
;; will return  f
(define (create-f df) ;; df is a decision function
  (define (f data)
    (map (lambda (x) (cdr x))
         (mypack
          (sort
           (map (lambda (x) (cons ((cdr df) (car x)) x)) data)
           < #:key car))))
  f)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;choose the decision function that most reduces entropy of the data
(provide choose-f)
(define (choose-f candidates data) ; returns a decision function
  (let*([list-f (map (lambda (x) (cons (create-f x) x)) candidates)] ;; list of (cons f decision-function)
        [cons-f-entropy (map (lambda (f+df) (cons (cdr f+df) (entropy-diff (car f+df) data)))
                             list-f)])
    (car (mymax cons-f-entropy (lambda (x y) (> (cdr x) (cdr y)))))))

(provide DTree)
(struct DTree (desc func kids) #:transparent)

;build a decision tree (depth limited) from the candidate decision functions and data
(provide build-tree)
(define (build-tree candidates data depth)
  (define (helper candidates data depth val)
    (let ([prob (get-leaf-prob data)])
      (cond [(or (= depth 0) (= prob 0)) (DTree (~a prob) (cons val '()) '())]
            [else (let*([cur-df (choose-f candidates data)]
                        [div-data ((create-f cur-df) data)]
                        [kids (map (lambda (d) (helper (remove cur-df candidates)
                                                       d (- depth 1) ((cdr cur-df) (caar d))))
                                   div-data)])
                    (DTree (car cur-df) (cons val (cdr cur-df)) kids))])))
  (helper candidates data depth 0))
;given a test data (features only), make a decision according to a decision tree
;returns probability of the test data being classified as 1
(provide make-decision)
(define (make-decision tree test)
  (match tree
    [(DTree des cons-fun '()) (string->number des)]
    [(DTree des cons-fun kids) (let*([b-code ((cdr cons-fun) test)];; b-code = branch code
                                     [corres-kid (get-kid kids b-code)])
                                 (if (null? corres-kid) 0
                                     (make-decision corres-kid test)))]))
(define (get-kid kids b-code)
  (if (null? kids) '()
      (match (car kids)
        [(DTree d (cons b f) k) (if (= b b-code) (car kids)
                                    (get-kid (cdr kids) b-code))])))

;============================================================================================================
;============================================================================================================
;============================================================================================================

;annotate list with indices
(define (pair-idx lst n)
  (if (empty? lst) `() (cons (cons (car lst) n) (pair-idx (cdr lst) (+ n 1))))
  )

;generate tree edges (parent to child) and recurse to generate sub trees
(define (dot-child children prefix tabs)
  (apply string-append (map (lambda (t) (string-append tabs "r" prefix "--" "r" prefix (~a (cdr t)) "[label=\"" (~a (cdr t)) "\"];" "\n" (dot-helper (car t) (string-append prefix (~a (cdr t))) (string-append tabs "\t")))) children))
  )

;generate tree nodes and call function to generate edges
(define (dot-helper tree prefix tabs)
  (let* ([node (match tree [(DTree d f c) (cons d c)])]
         [d (car node)]
         [c (cdr node)])
    (string-append tabs "r" prefix "[label=\"" d "\"];" "\n\n" (dot-child (pair-idx c 0) prefix tabs))
    )
  )

;output tree (dot file)
(provide display-tree)
(define (display-tree tree dtfile)
  (write-file dtfile (string-append "graph \"decision-tree\" {" "\n" (dot-helper tree "" "\t") "}"))
  )
;============================================================================================================
;============================================================================================================
;============================================================================================================

;(define toy-t (build-tree (list y1 y2 y3 y4>62) toy 3))
;(display-tree toy-t "toy.dot")
;  
;(define titanic-t (build-tree (list pclass sex age>25 sibsp parch fare>50 emb) titanic 5))
;(display-tree titanic-t "titanic.dot")
;  
;(define mushroom-t (build-tree (list cshape csurf bruise odor gatch gspace gsize sshape nring pop hab)
;                            mushroom 8))
;(display-tree mushroom-t "mushroom.dot")
;
;(define toy-test (map (lambda (d) (map string->number d)) (cdr (read-csv-file "../data/toy_test.csv"))))
;(define titanic-test (map (lambda (d) (map string->number (cddr d))) (cdr (read-csv-file "../data/titanic_test.csv"))))
;(define mushroom-test (map (lambda (d) (map string->number d)) (cdr (read-csv-file "../data/mushrooms_test.csv"))))
