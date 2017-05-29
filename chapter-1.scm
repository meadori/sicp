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


;; Exercise 1.6

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(write (sqrt 9))
(newline)
;; 3.00009155413138

(write (sqrt (+ 100 37)))
(newline)
;; 11.704699917758145

(write (sqrt (+ (sqrt 2) (sqrt 3))))
(newline)
;; 1.7739279023207892

(write (square (sqrt 1000)))
(newline)
;; 1000.000369924366

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(write (new-if (= 2 3) 0 5))
(newline)
;; 5

(write (new-if (= 1 1) 0 5))
(newline)
;; 0

;; Now, consider defining `sqrt-iter` in terms of
;; `new-if`:
;;
;;   (define (sqrt-iter guess x)
;;     (new-if (good-enough? guess x)
;;         guess
;;         (sqrt-iter (improve guess x)
;;                    x)))
;;
;; This will cause an infinite loop because `new-if` is
;; applied as a function call, thus its arguments are
;; evaluated before it is called.  As such, `sqrt-iter`
;; is applied over-and-over and the base case is never
;; triggered.
;;
;; This is a good example of why special forms are needed.


;; Exericse 1.7

;; `sqrt` fails for:
;;
;;   (sqrt 1000000000000000)
;;
;; The execution trace of guesses looks like:
;;
;;   1.0
;;   500000000000000.5
;;   250000000000001.25
;;   125000000000002.62
;;   62500000000005.31
;;   31250000000010.656
;;   15625000000021.328
;;   7812500000042.664
;;   3906250000085.332
;;   1953125000170.666
;;   976562500341.333
;;   488281250682.6665
;;   244140626365.33325
;;   122070315230.66661
;;   61035161711.33321
;;   30517589047.665874
;;   15258810907.827074
;;   7629438221.866625
;;   3814784646.558015
;;   1907523392.2766902
;;   954023816.1217878
;;   477536003.99178225
;;   239815043.46673402
;;   121992461.83041875
;;   65094844.81723103
;;   40228522.216194235
;;   32543253.585447475
;;   31635794.320938785
;;   31622779.27999515
;;   31622776.601683907
;;   31622776.601683795
;;   31622776.601683795
;;   31622776.601683795
;;   ...
;;
;; As seen, it bottoms out at 31622776.601683795.
;; `good-enough` returns false for this number because
;; its value squared minus the radican is *not* less
;; than 0.001:
;;
;;   > (- (square 31622776.601683795) 1000000000000000)
;;   0.125
;;
;; Then the guess is "improved":
;;
;;   > (average 31622776.601683795 (/ 1000000000000000 31622776.601683795))
;;   31622776.601683795
;;
;; However, it is "improved" to the exact same number.  Thus the
;; process goes on forever.
;;
;; Now consider taking the square root of a very small number:
;;
;;   > (sqrt 1.8e-10)
;;   0.031250001918124985
;;
;; It does terminate, but the result is away from the actual result of
;; 1.3416407864998738e-05:
;;
;;   > (- 0.031250001918124985 1.3416407864998738e-05)
;;   0.031236585510259988

(define (sqrt-iter prev-guess guess x)
  (if (good-enough? prev-guess guess)
      guess
      (sqrt-iter guess (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? prev-guess guess)
  (< (abs (- prev-guess guess)) 0.000001))

(define (sqrt x)
  (sqrt-iter 0.0 1.0 x))

;; The above method where successive guesses are compared
;; gives much better results:
;;
;;   > (sqrt 1000000000000000)
;;   31622776.601683795
;;   > (sqrt 1.8e-10)
;;   1.3440077688603855e-05
;;
;; It actually terminates for the large case and gives a much
;; closer result for the smaller number.


;; Exericse 1.8

(define (cube-root x)
  (define (cube-root-iter prev-guess guess x)
    (if (good-enough? prev-guess guess)
        guess
        (cube-root-iter guess (improve guess x)
                        x)))
  (define (improve guess x)
    (/ (+ (/ x (square guess))
          (* 2 guess))
       3))
  (define (good-enough? prev-guess guess)
    (< (abs (- prev-guess guess)) 0.000001))
  (cube-root-iter 0 1.0 x))
;; > (cube-root 9)
;; 2.080083823051904
;; > (cube-root 27)
;; 3.0000000000000977
;; > (cube-root 10000000000000)
;; 21544.346900318837


;; Exercise 1.9

;; Consider:
;;
;;   (define (+ a b)
;;     (if (= a 0)
;;         b
;;         (inc (+ (dec a) b))))
;;
;; Evaluate `(+ 4 5)`:
;;
;;   (+ 4 5)
;;   (inc (+ 3 5))
;;   (inc (inc (+ 2 5)))
;;   (inc (inc (inc (+ 1 5))))
;;   (inc (inc (inc (inc (+ 0 5)))))
;;   (inc (inc (inc (inc 5))))
;;   (inc (inc (inc 6)))
;;   (inc (inc 7))
;;   (inc 8)
;;   9
;;
;; This is a linear recursive process.
;;
;; Consider:
;;
;;   (define (+ a b)
;;     (if (= a 0)
;;         b
;;         (+ (dec a) (inc b))))
;;
;; Evaluate `(+ 4 5)`:
;;
;;   (+ 4 5)
;;   (+ 3 6)
;;   (+ 2 7)
;;   (+ 1 8)
;;   (+ 0 9)
;;   9
;;
;; This is a linear iterative process.


;; Exercise 1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10)
;; 1024

(A 2 4)
;; 65536

(A 3 3)
;; 65536

(define (f n) (A 0 n))
;; f(n) = 2 * n

(define (g n) (A 1 n))
;; g(n) = 2 ^ n

(define (h n) (A 2 n))
;; h(1) = 2
;; h(n) = 2 ^ h(n - 1)

(define (k n) (* 5 n n))
;; k(n) = 5 * n ^ 2


;; Exercise 1.11

(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

(write (f 2))
(newline)
;; 2

(write (f 3))
(newline)
;; 4

(write (f 11))
(newline)
;; 4489

(define (f n)
  (define (f-iter a b c n)
    (if (= n 0)
        c
        (f-iter (+ a (* 2 b) (* 3 c)) a b (- n 1))))
  (f-iter 2 1 0 n))

(write (f 2))
(newline)
;; 2

(write (f 3))
(newline)
;; 4

(write (f 11))
(newline)
;; 4489


;; Exercise 1.12

(define (pascal row col)
  (cond ((= col 1) 1)
        ((= col row) 1)
        (else (+ (pascal (- row 1) (- col 1))
                 (pascal (- row 1) col)))))

(write (pascal 3 2))
(newline)
;; 2

(write (pascal 5 3))
(newline)
;; 6


;; Exercise 1.13

;; Given:
;;
;;   fib(n) = fib(n - 1) + fib(n - 2)
;;   phi = (1 + sqrt(5)) / 2
;;   psi = (1 - sqrt(5)) / 2
;;
;; Also note that:
;;
;;   phi^n = phi^(n-1) + phi^(n-2)
;;   psi^n = psi^(n-1) + psi^(n-2)
;;
;; Then, by induction on `n` the following can be proved:
;;
;;   fib(n) = (phi^n - psi^n) / sqrt(5)
;;
;; Base cases:
;;
;;   fib(0) = 0 = (phi^0 - psi^0) / sqrt(5) = 0
;;   fib(1) = 1 = (phi^1 - psi^1) / sqrt(5)
;;          = ((1 + sqrt(5))/2 - (1 - sqrt(5))/2) / sqrt(5)
;;          = ((2 * sqrt(5)) / 2) / sqrt(5) = 1
;;
;; Induction step:
;;
;;   fib(n) = fib(n - 1) + fib(n - 2)
;;          = ((phi^(n-1) - psi^(n-1)) + (phi^(n-2) - psi^(n-2))) / sqrt(5)
;;          = ((phi^(n-1) + phi^(n-2)) - (psi^(n-1) + psi^(n-2))) / sqrt(5)
;;          = (phi^n - psi^n) / sqrt(5)

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(write (count-change 11))
(newline)

;; Exercise 1.14

;; The call tree for `(count-change 11) looks like:
;;
;; (cc 11 5)                                = 4
;;   (cc 11 4)                              = 4
;;     (cc 11 3)                            = 4
;;       (cc 11 2)                          = 3
;;         (cc 11 1)                        = 2
;;           (cc 11 0)                      = 0
;;           (cc 10 1)                      = 1
;;             (cc 10 0)                    = 0
;;             (cc  9 1)                    = 1
;;               (cc 9 0)                   = 0
;;               (cc 8 1)                   = 1
;;                 (cc 7 0)                 = 0
;;                 (cc 6 1)                 = 1
;;                   (cc 5 0)               = 0
;;                   (cc 4 1)               = 1
;;                     (cc 4 0)             = 0
;;                     (cc 3 1)             = 1
;;                       (cc 3 0)           = 0
;;                       (cc 2 1)           = 1
;;                         (cc 2 0)         = 0
;;                         (cc 1 1)         = 1
;;                           (cc 1 0)       = 0
;;                           (cc 0 1)       = 1
;;         (cc 6 2)                         = 1
;;           (cc 6 1)                       = 0
;;             (cc 6 0)                     = 0
;;           (cc 1 2)                       = 1
;;             (cc 1 1)                     = 0
;;               (cc 1 0)                   = 0
;;             (cc 0 1)                     = 1
;;       (cc 1 3)                           = 1
;;         (cc 1 2)                         = 1
;;           (cc 1 1)                       = 1
;;             (cc 1 0)                     = 0
;;             (cc 0 1)                     = 1
;;           (cc -4 2)                      = 0
;;         (cc -9 3)                        = 0
;;     (cc -14 4)                           = 0
;;   (cc -38 5)                             = 0
;;
;; As the amount of change, `ac`, increases the space
;; complexity is O(ac).  The runtime complexity is
;; exponential, O(c^ac), for some constant `c`.
