;----------------------------------------------------------
; Problem Set #1
; Date: March 03, 2020.
; Authors:
;          A01378562 Karla Daniela López Vega
;          A01371925 Luis Eduardo Díaz Niño de Rivera
;----------------------------------------------------------

(require '[clojure.test :refer [deftest is run-tests]])
(require '[clojure.math.numeric-tower :refer [sqrt]])
(require '[clojure.math.numeric-tower :refer [abs]])




(defn !
  "Computes the factorial of n using explicit recursion."
  [n]
  (if (zero? n)
    1
    (*' n (! (dec n)))))
; n = 9
; 2! = 2 * 1!
; 1! = 1 * 0!
; 0! = 1

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

(deftest test-duplicate
  (is (= '(1 1 2 2 3 3 4 4 5 5)
         (duplicate '(1 2 3 4 5))))
  (is (= ()
         (duplicate ())))
  (is (= '(a a)
         (duplicate '(a))))
  (is (= '(a a b b c c d d e e f f g g h h)
         (duplicate '(a b c d e f g h)))))

(defn fib
  "Computes the n-th element in the Fibonacci sequence "
  [n]
  (if (<= n 1)
    n
    (+' (fib (- n 1)) (fib (- n 2)))))

(defn pow
  "Takes two arguments as input: a number a and a positive
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
          (reduce *' (repeat b a)))))))

; a = 1 : b 3
; (1 1 1)

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
  "Takes a positive integer n as its argument and
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
  "Surrounds in a list every upper-level element of the list it
   takes as input"
  [lst]
  (mapcat #(list (list %)) lst))

; '(a b c) ((a) (b) (c)))

(deftest test-enlist
  (is (= () (enlist ())))
  (is (= '((a) (b) (c)) (enlist '(a b c))))
  (is (= '(((1 2 3)) (4) ((5)) (7) (8))
         (enlist '((1 2 3) 4 (5) 7 8)))))

(defn positives
  "Takes a list of numbers lst as its argument, and returns a new
   list that only contains the positive numbers of lst"
  [lst]
  (remove #(< % 0) lst))

; (-1 2) (2)

(deftest test-positives
  (is (= () (positives '())))
  (is (= () (positives '(-4 -1 -10 -13 -5))))
  (is (= '(3 6) (positives '(-4 3 -1 -10 -13 6 -5))))
  (is (= '(4 3 1 10 13 6 5) (positives '(4 3 1 10 13 6 5)))))

(defn add-list
  "Returns the sum of all the elements of its input list, or 0 if its empty"
  [lst]
  (reduce + lst))

(deftest test-add-list
  (is (= 0 (add-list ())))
  (is (= 10 (add-list '(2 4 1 3))))
  (is (= 55 (add-list '(1 2 3 4 5 6 7 8 9 10)))))

(defn invert-pairs
  "Takes as an argument a list of vectors containing two elements each. It
   returns a new list with every vector pair inverted."
  [lst]
  (map reverse lst))

; ([a 1][a 2][b 1][b 2]) - > ([1 a][2 a][1 b][2 b])

(deftest test-invert-pairs
  (is (= () (invert-pairs ())))
  (is (= '([1 a][2 a][1 b][2 b]))
      (invert-pairs '([a 1][a 2][b 1][b 2])))
  (is (= '([1 January][2 February][3 March])
         (invert-pairs '([January 1][February 2][March 3])))))

(defn list-of-symbols?
  "The function list-of-symbols? takes a list lst as its argument.
   It returns true if all the elements (possibly zero) contained in lst are symbols,
   or false otherwise."
  [lst]
  (every? symbol? lst))

(deftest test-list-of-symbols?
  (is (list-of-symbols? ()))
  (is (list-of-symbols? '(a)))
  (is (list-of-symbols? '(a b c d e)))
  (is (not (list-of-symbols? '(a b c d 42 e))))
  (is (not (list-of-symbols? '(42 a b c)))))

(defn swapper
  "Takes a list lst as its argument. It returns true if all the elements (possibly zero)
   contained in lst are symbols, or false otherwise. "
  [a b lst]
  (map
    (fn [e]
      (cond
        (= e a) b
        (= e b) a
        :else e))
    lst))

(deftest test-swapper
  (is (= ()
         (swapper 1 2 ())))
  (is (= '(4 3 4 9 9 3 3 3 9 9 7 9
            3 7 8 7 8 4 5 6)
         (swapper 1 2 [4 3 4 9 9 3 3 3 9 9 7
                       9 3 7 8 7 8 4 5 6])))
  (is (= '(4 4 5 1 4 8 1 5 6 4 5 2 9 5 9 9 2 1 1 4)
         (swapper 1 2 [4 4 5 2 4 8 2 5 6 4 5
                       1 9 5 9 9 1 2 2 4])))
  (is (= '(soft purr warm purr little ball of fur
                happy purr sleepy purr kitty kitty kitty)
         (swapper 'purr
                  'kitty
                  '(soft kitty warm kitty little ball
                         of fur happy kitty sleepy kitty
                         purr purr purr)))))

(defn dot-product
  "The lists a and b. It returns the result of performing the dot product of a times b.
   The dot product is an algebraic operation that takes two equal-length sequences of
   numbers and returns a single number obtained by multiplying corresponding entries and
   then summing those products"
  [a b]
  (reduce +' (map *' a b)))

(deftest test-dot-product
  (is (= 0 (dot-product () ())))
  (is (= 32 (dot-product '(1 2 3) '(4 5 6))))
  (is (= 21.45 (dot-product '(1.3 3.4 5.7 9.5 10.4)
                            '(-4.5 3.0 1.5 0.9 0.0)))))

(defn average
  "The function average takes a list lst as its argument. It returns the arithmetic mean
   of the numbers contained in lst, or nil if lst is empty"
  [lst]
  (cond
    (= (count lst) 0) nil
    :else (/ (reduce +' lst) (count lst))))

(deftest test-average
  (is (nil? (average ())))
  (is (= 4
         (average '(4))))
  (is (= 3
         (average '(5 6 1 6 0 1 2))))
  (is (= 2.5
         (average '(1.7 4.5 0 2.0 3.4 5 2.5 2.2 1.2)))))


(defn standard-deviation
  [lst]
  "The function standard-deviation takes a list lst as its argument.
   It returns the population standard deviation of the numbers contained in lst,
   or nil if lst is empty."
  (cond
    (= (count lst) 0) nil
    :else (sqrt (/ (reduce + (map
                               (fn [e]
                                 (*' (- e (average lst)) (- e (average lst))))
                               lst)) (count lst)))))

(defn aprox=
  "Checks if x is approximately equal to y. Returns true
  if |x - y| < epsilon, or false otherwise."
  [epsilon x y]
  (< (abs (- x y)) epsilon))


(deftest test-standard-deviation
  (is (nil? (standard-deviation ())))
  (is (aprox= 0.01
              1.87
              (standard-deviation
                '(6 2 3 1))))
  (is (aprox= 0.0001
              12.3153
              (standard-deviation
                '(4 8 15 16 23 42))))
  (is (aprox= 0.00001
              7.07106
              (standard-deviation
                '(110 105 90 100 95))))
  (is (aprox= 0.001
              2.983
              (standard-deviation
                '(9 2 5 4 12 7 8 11
                   9 3 7 4 12 5 4 10
                   9 6 9 4)))))

(run-tests)
