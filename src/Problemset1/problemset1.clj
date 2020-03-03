;----------------------------------------------------------
; Problem Set #1
; Date: March 03, 2020.
; Authors:
;          A01378562 Karla Daniela López Vega
;          A01371925 Luis Eduardo Díaz Niño de Rivera
;----------------------------------------------------------

(require '[clojure.test :refer [deftest is run-tests]])

(defn !
  "Computes the factorial of n using explicit recursion."
  [n]
  (if (zero? n)
    1
    (*' n (! (dec n)))))

(deftest test-!
  (is (= 1
         (! 0)))
  (is (= 120
         (! 5)))
  (is (= '(1 1 2 6 24 120 720 5040 40320 362880 3628800)
         (map ! (range 11))))
  (is (= 15511210043330985984000000N
         (! 25)))
  (is (= 815915283247897734345611269596115894272000000000N
         (! 40))))

(defn duplicate
  "Duplicates each element in lst."
  [lst]
  (mapcat #(list % %) lst))

(defn fib
  "Cmputes the n-th element in the Fibonacci sequence "
  [n]
  (if (<= n 1)
    n
    (+' (fib (- n 1)) (fib (- n 2)))))

(defn pow
  "takes two arguments as input: a number a and a positive
   integer b. It returns the result of computing a raised
   to the power b"
  [a
   b]
  (if (and (zero? a) (zero? b))
    1
    (if (and (zero? a) (= b 1))
      0
      (if (and (> a 0) (= b 0))
        1
        (if (= b 1)
          a
          (reduce *' (repeat b a))))
      )))

(deftest test-pow
  (is (= 1 (pow 0 0)))
  (is (= 0 (pow 0 1)))
  (is (= 1 (pow 5 0)))
  (is (= 5 (pow 5 1)))
  (is (= 125 (pow 5 3)))
  (is (= 25 (pow -5 2)))
  (is (= -125 (pow -5 3)))
  (is (= 1024 (pow 2 10)))
  (is (= 525.21875 (pow 3.5 5)))
  (is (= 129746337890625 (pow 15 12)))
  (is (= 3909821048582988049 (pow 7 22))))

(defn fib
  [n]
  "takes a positive integer n as its argument and
   returns the corresponding element of the Fibonacci sequence"
  (if ( <= n 1)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(deftest test-fib
  (is (= 0
         (fib 0)))
  (is (= 1
         (fib 1)))
  (is (= 1
         (fib 2)))
  (is (= 5
         (fib 5)))
  (is (= '(0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610
            987 1597 2584 4181 6765)
         (map fib (range 21))))
  (is (= 267914296
         (fib 42))))

(defn enlist
  "surrounds in a list every upper-level element of the list it
   takes as input"
  [lst]
  (mapcat #(list (list %) ) lst))

(deftest test-enlist
  (is (= () (enlist ())))
  (is (= '((a) (b) (c)) (enlist '(a b c))))
  (is (= '(((1 2 3)) (4) ((5)) (7) (8))
         (enlist '((1 2 3) 4 (5) 7 8)))))

(run-tests)