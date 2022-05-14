
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
(define sequence
  (lambda (low high stride)
    (if (> low high)
        null
        (cons low
              (sequence (+ low stride) high stride)))))



(define (string-append-map xs suffix)
  (map (lambda (x)
         (string-append x suffix))
       xs))


(define (list-nth-mod xs n)
  (cond ((< n 0)
         (error "list-nth-mod: negative number"))
        ((empty? xs)
         (error "list-nth-mod: empty list"))
        (else (car (list-tail xs
                              (remainder n (length xs)))))))

(define (stream-for-n-steps s n)
  (let ([x (s)])
    (if (= n 0)
        empty
        (cons (car x)
              (stream-for-n-steps (cdr x)
                                  (- n 1))))))

(define (funny-number-stream)
  (define (f i)
    (lambda ()
      (if (= (remainder i 5) 0)
          (cons (- 0 i)
              (f (+ i 1)))
          (cons i (f (+ i 1))))))
  ((f 1)))

(define dan-then-dog
  (lambda ()
    (define (f i)
      (if (= (remainder i 2) 0)
          (cons "dan.jpg" (lambda () (f (+ i 1))))
          (cons "dog.jpg" (lambda () (f (+ i 1))))))
    (f 0)))

(define (stream-add-zero s)
  (lambda ()
    (let ([x (s)])
      (cons (cons 0 (car x))
            (cdr x)))))

(define (cycle-lists xs ys)
  (define (helper k)
    (let ([x (list-nth-mod xs k)]
          [y (list-nth-mod ys k)])
      (cons (cons x y)
            (lambda () (helper (+ k 1))))))
    (lambda () (helper 0)))

(define (vector-assoc v vec)
    (define (iter i n)
        (if (= i n) 
            #f
            (let ([x (vector-ref vec i)])
                (if (equal? v (car x))
                    x
                    (iter (+ i 1) n)))))
    (iter 0 (vector-length vec)))

(define (cached-assoc xs n)
    (letrec ([cache (make-vector n (cons #f #f))]
        [i 0]
        [f (lambda (v) 
                   (let ([x (vector-assoc v cache)])
                        (if x
                            x
                            (let ([y (assoc v xs)])
                                (if y
                                    (begin (vector-set! cache i y)
                                        (set! i (remainder (+ i 1) n))
                                        y)
                                    #f)))))])
    f))