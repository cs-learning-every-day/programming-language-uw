;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1
(define (racketlist->mupllist ls)
  (if (null? ls) 
      (aunit)
      (apair (car ls) (racketlist->mupllist (cdr ls)))))

;; CHANGE (put your solutions here)
(define (mupllist->racketlist ls)
  (if (aunit? ls)
      null
      (cons (apair-e1 ls)
            (mupllist->racketlist (apair-e2 ls)))))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(int? e) e]
        [(ifgreater? e)
          (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
                [v2 (eval-under-env (ifgreater-e2 e) env)])
            (if (and (int? v1)
                     (int? v2))
                (if (>  (int-num v1)
                        (int-num v2))
                    (eval-under-env (ifgreater-e3 e) env)
                    (eval-under-env (ifgreater-e4 e) env))
                (error "MUPL ifgreater e1 e2 applied to non-number")))]
        [(mlet? e) 
          (let ([key (mlet-var e)]
                [value (eval-under-env (mlet-e e) env)])
            (eval-under-env (mlet-body e) 
                            (append (list (cons key value)) env)))]
        [(apair? e) 
          (let ([v1 (eval-under-env (apair-e1 e) env)]
                [v2 (eval-under-env (apair-e2 e) env)])
            (apair v1 v2))]
        [(fst? e) 
          (let ([v (eval-under-env (fst-e e) env)])
            (if (apair? v)
                (apair-e1 v)
                (error "MUPL fst should applied to pair")))]
        [(snd? e) 
          (let ([v (eval-under-env (snd-e e) env)])
            (if (apair? v)
                (apair-e2 v)
                (error "MUPL snd should applied to pair")))]
        [(isaunit? e) 
          (let ([v (eval-under-env (isaunit-e e) env)])
            (if (aunit? v)
                (int 1)
                (int 0)))]
        [(fun? e) (closure env e)]
        [(closure? e) e]
        [(call? e)
          (let ([fc (eval-under-env (call-funexp e) env)]
                [arg (eval-under-env (call-actual e) env)])
            (if (closure? fc)
                (let* ([ff (closure-fun fc)]
                       [fenv (append (list (cons (fun-formal ff) arg)) 
                                     (closure-env fc))]
                       [fname (fun-nameopt ff)])
                      (if fname
                        (eval-under-env (fun-body ff) (append (list (cons fname fc)) 
                                                              fenv))
                        (eval-under-env (fun-body ff) fenv)))
                (error "MUPL call first arg is not func")))]
        [(aunit? e) e]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3) 
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (let ([kp (car lstlst)])
          (mlet (car kp) 
                (cdr kp)
                (mlet* (cdr lstlst) e2)))))

(define (ifeq e1 e2 e3 e4)
  (mlet "_y" e4
    (ifgreater e1 e2 (var "_y")
      (mlet "_x" e3 (ifgreater e2 e1 (var "_y") (var "_x"))))))

;; Problem 4

(define mupl-map 
  (fun #f "f"
       (fun #f
            "lst"
            (call (fun "helper-f" "ls"
                       (ifaunit (var "ls")
                                (var "ls")
                                (apair (call (var "f") (fst (var "ls")))
                                       (call (var "helper-f") (snd (var "ls"))))))
                  (var "lst")))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "x"
             (call (var "map") (fun #f "y" (add (var "y") (var "x")))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))