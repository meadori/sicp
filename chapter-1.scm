;;; Exercise 1.1

10
;; 10

(+ 5 3 4)
;; 12

(- 9 1)
;; 8

(/ 6 2)
;; 3

(+ (* 2 4) (- 4 6))
;; 6

(define a 3)
;; No result.

(define b (+ a 1))
;; No result.

(+ a b (* a b))
;; 19

(= a b)
;; #f

(if (and (> b a) (< b (* a b)))
    b
    a)
;; 4

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
;; 16

(+ 2 (if (> b a) b a))
;; 6

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))
;; 16


;;; Exercise 1.2

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))


;;; Exercise 1.3

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (max-sum-of-squares x y z)
  (if (< x y)
      ;; (x < y)
      (if (< x z)
          ;; (x < y) and (x < z)
          (sum-of-squares y z)
          ;; (x < y) and (x >= z)
          (sum-of-squares x y))
      ;; (x >= y)
      (if (> y z)
          ;; (x >= y) and (> y z)
          (sum-of-squares x y)
          ;; (x >= y) and (y <= z)
          (sum-of-squares x z))))


;;; Exercise 1.4

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
;; If b is positive, then the result is a + b.
;; Otherwise, the result is a - b, which is
;; equivalent to a + |b|.  This works because
;; functions can be treated as values.


;; Exercise 1.5

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))
;; Consider:
;;
;;   (test 0 (p))
;;
;; For applicative-order evaluation, this will cause an
;; infinite loop because the arguments to `test` will be
;; evaluated *before* `test` is called and `p` is infinitely
;; recursive. 
;;
;; For normal-order evaluation, `0` will be returned because
;; the arguments are *not* evaluated until they are used --
;; lazy evaluation.
