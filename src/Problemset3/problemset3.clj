;----------------------------------------------------------
; Problem Set #3
; Date: April 1, 2020.
; Authors:
;          A01378562 Karla Daniela López Vega
;          A01371925 Luis Eduardo Díaz Niño de Rivera
;----------------------------------------------------------

(require '[clojure.test :refer [deftest is run-tests]])
(require '[clojure.math.numeric-tower :refer [abs]])

(def ^:dinamic n 1)
(defn alpha
      ([] (* n 2))
      ([n] (* n 2)))
(+ (alpha)
   (binding [n 2]
     (+ (alpha) (alpha 4))))

(println n)

(defn aprox=
  "Checks if x is approximately equal to y. Returns true
  if |x - y| < epsilon, or false otherwise."
  [epsilon x y]
  (< (abs (- x y)) epsilon))

(defn argswap
  "Takes as input a two argument function f and returns a
   new function that behaves like f but with the order of
   its two arguments swapped."
  [f]
  (fn [a b]
    (f b a)))

(deftest test-argswap
  (is (= '(2 1)
         ((argswap list) 1 2)))
  (is (= -7
         ((argswap -) 10 3)))
  (is (= 1/4
         ((argswap /) 8 2)))
  (is (= '((4 5 6) 1 2 3)
         ((argswap cons) '(1 2 3) '(4 5 6))))
  (is (= '(1 0 4 25 100)
         ((argswap map) '(-1 0 2 5 10) #(* % %)))))

(defn there-exists-one
  [pred
   lst]
  "Takes two arguments: a one argument predicate function
   pred and a list lst. Returns true if there is exactly
   one element in lst that satisfies pred, otherwise returns
   false"
  (some pred lst))

(deftest test-there-exists-one
  (is (not (there-exists-one pos?
                             ())))
  (is (there-exists-one pos?
                        '(-1 -10 4 -5 -2 -1)))
  (is (there-exists-one neg?
                        '(-1)))
  (is (not (there-exists-one symbol?
                             '(4 8 15 16 23 42))))
  (is (there-exists-one symbol?
                        '(4 8 15 sixteen 23 42))))


(defn linear-search
  [vct
   x
   eq-fun]
  "takes three arguments: a vector vct, a data value x, and
   an equality function eq-fun. It sequentially searches for
   x in vct using eq-fun to compare x with the elements
   contained in vct. The eq-fun should accept two arguments,
   a and b, and return true if a is equal to b, or false
   otherwise."
  (->>
    vct
    (map-indexed vector)
    (filter #(eq-fun x (second %)))
    (first)
    (first)))

(deftest test-linear-search
  (is (nil? (linear-search [] 5 =)))
  (is (= 0 (linear-search [5] 5 =)))
  (is (= 4 (linear-search
             [48 77 30 31 5 20 91 92
              69 97 28 32 17 18 96]
             5
             =)))
  (is (= 3 (linear-search
             ["red" "blue" "green" "black" "white"]
             "black"
             identical?)))
  (is (nil? (linear-search
              [48 77 30 31 5 20 91 92
               69 97 28 32 17 18 96]
              96.0
              =)))
  (is (= 14 (linear-search
              [48 77 30 31 5 20 91 92
               69 97 28 32 17 18 96]
              96.0
              ==)))
  (is (= 8 (linear-search
             [48 77 30 31 5 20 91 92
              69 97 28 32 17 18 96]
             70
             #(<= (abs (- %1 %2)) 1)))))

(defn bisection
  "Write the function bisection, that takes a, b, and f as arguments. It finds the corresponding root using the bisection
  method. The algorithm must stop when a value of c is found such that:|f(c)| < 1.0 × 10−15."
  [a b f]
  (loop [a  a
         b  b
         c  (* (/ (+ a b) 2) 1.0)  ]
    (if(< (abs (f c)) 1.0E-15)
      c
      (if(= (neg? (f a)) (neg? (f c)))
        (recur c b (* (/ (+ a b) 2) 1.0))
        (recur a c (* (/ (+ a b) 2) 1.0))
        ))
    ))

(deftest test-bisection
  (is (aprox= 0.0001
              3.0
              (bisection 1 4 (fn [x] (* (- x 3) (+ x 4))))))
  (is (aprox= 0.0001
              -4.0
              (bisection -5 0 (fn [x] (* (- x 3) (+ x 4))))))
  (is (aprox= 0.0001
              Math/PI
              (bisection 1 4 (fn [x] (Math/sin x)))))
  (is (aprox= 0.0001
              (* 2 Math/PI)
              (bisection 5 10 (fn [x] (Math/sin x)))))
  (is (aprox= 0.0001
              1.618033988749895
              (bisection 1 2 (fn [x] (- (* x x) x 1)))))
  (is (aprox= 0.0001
              -0.6180339887498948
              (bisection -10 1 (fn [x] (- (* x x) x 1))))))

(defn deriv
"The function takes f and h as its arguments, and returns a new function that takes x as argument, and which represents
the derivative of f given a certain value for h."
  [f
   h]
  (fn
    [x]
    (/ (- (f (+ x h)) (f x)) h)))

(defn f1 [x] (* x x x))
(def df (deriv f1 0.001))
(def ddf (deriv df 0.001))
(def dddf (deriv ddf 0.001))

(deftest test-deriv
  (is (aprox= 0.05 75 (df 5)))
  (is (aprox= 0.05 30 (ddf 5)))
  (is (aprox= 0.05 6 (dddf 5))))

(defn newton
  "Takes f and n as its arguments returns the corresponding value of Xn."
  [f n]
  (cond
    (= n 0) 0
    :else (let [x (newton f (dec n))]
            (- x
               (/ (f x)
                  ((deriv f 0.001) x))))))

(deftest test-newton
  (is (aprox= 0.00001
              10.0
              (newton (fn [x] (- x 10))
                      1)))
  (is (aprox= 0.00001
              -0.5
              (newton (fn [x] (+ (* 4 x) 2))
                      1)))
  (is (aprox= 0.00001
              -1.0
              (newton (fn [x] (+ (* x x x) 1))
                      50)))
  (is (aprox= 0.00001
              -1.02987
              (newton (fn [x] (+ (Math/cos x)
                                 (* 0.5 x)))
                      5))))

(defn binary-search
  "takes three arguments: a vector vct sorted in ascending order and with no repeated
   elements, a data value x, and a less than function lt-fun. It implements the binary
   search algorithm, searching for x in vct using the lt-fun to compare x with the
   elements contained in vct. The lt-fun should accept two arguments, a and b,
    and return true if a is less than b, or false otherwise."
  [vct x lt-fun]
    (loop [start 0
           end (dec (count vct))]
      (cond
        (lt-fun end start) nil
        (= x (nth vct (quot (+ start end) 2))) (quot (+ start end) 2)
        (lt-fun x (nth vct (quot (+ start end) 2))) (recur start (dec (quot (+ start end) 2)))
        ((fn [a b] (lt-fun b a)) x (nth vct (quot (+ start end) 2))) (recur (inc (quot (+ start end) 2)) end))))

(def small-list [4 8 15 16 23 42])

(def big-list [0 2 5 10 11 13 16 20 24 26
               29 30 31 32 34 37 40 43 44
               46 50 53 58 59 62 63 66 67
               70 72 77 79 80 83 85 86 94
               95 96 99])

(def animals ["dog" "dragon" "horse" "monkey" "ox"
              "pig" "rabbit" "rat" "rooster" "sheep"
              "snake" "tiger"])
(defn str<
  "Returns true if a is less than b, otherwise
   returns false. Designed to work with strings."
  [a b]
  (< (compare a b) 0))

(deftest test-binary-search
  (is (nil? (binary-search [] 5 <)))
  (is (= 3 (binary-search small-list 16 <)))
  (is (= 0 (binary-search small-list 4 <)))
  (is (= 5 (binary-search small-list 42 <)))
  (is (nil? (binary-search small-list 7 <)))
  (is (nil? (binary-search small-list 2 <)))
  (is (nil? (binary-search small-list 99 <)))
  (is (= 17 (binary-search big-list 43 <)))
  (is (= 0 (binary-search big-list 0 <)))
  (is (= 39 (binary-search big-list 99 <)))
  (is (nil? (binary-search big-list 12 <)))
  (is (nil? (binary-search big-list -1 <)))
  (is (nil? (binary-search big-list 100 <)))
  (is (= 5 (binary-search animals "pig" str<)))
  (is (= 0 (binary-search animals "dog" str<)))
  (is (= 11 (binary-search animals "tiger" str<)))
  (is (nil? (binary-search animals "elephant" str<)))
  (is (nil? (binary-search animals "alligator" str<)))
  (is (nil? (binary-search animals "unicorn" str<))))

(run-tests)
