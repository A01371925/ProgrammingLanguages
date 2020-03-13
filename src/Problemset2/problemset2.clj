;----------------------------------------------------------
; Problem Set #2
; Date: March 13, 2020.
; Authors:
;          A01378562 Karla Daniela López Vega
;          A01371925 Luis Eduardo Díaz Niño de Rivera
;----------------------------------------------------------

(require '[clojure.test :refer [deftest is run-tests]])

(defn replic
  [n
   lst]
  "It returns a new list that replicates n times each element contained in lst."
  (mapcat (fn [e]
            (repeat n e)) lst))

(deftest test-replic
  (is (= () (replic 7 ())))
  (is (= () (replic 0 '(a b c))))
  (is (= '(a a a) (replic 3 '(a))))
  (is (= '(1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4)
         (replic 4 '(1 2 3 4)))))

(defn expand
  [lst]
  "It returns a list where the first element of lst appears one time, the second
   elements appears two times, the third element appears three times, and so on"
  (mapcat (fn [e i]
            (repeat i e)) lst (range 1 (+ 1 (count lst)))))

(deftest test-expand
  (is (= () (expand ())))
  (is (= '(a) (expand '(a))))
  (is (= '(1 2 2 3 3 3 4 4 4 4) (expand '(1 2 3 4))))
  (is (= '(a b b c c c d d d d e e e e e)
         (expand '(a b c d e)))))

(defn insert
  [n
   lst]
  "It returns a new list with the same elements as lst but inserting n in its
   corresponding place"
  (sort (cons n lst)))

(deftest test-insert
  (is (= '(14) (insert 14 ())))
  (is (= '(4 5 6 7 8) (insert 4 '(5 6 7 8))))
  (is (= '(1 3 5 6 7 9 16) (insert 5 '(1 3 6 7 9 16))))
  (is (= '(1 5 6 10) (insert 10 '(1 5 6)))))

(defn- bubble [ys x]
  (if-let [y (peek ys)]
    (if (> y x)
      (conj (pop ys) x y)
      (conj ys x))
    [x]))

(defn my-sort [xs]
  "Takes an unordered list of numbers as an argument, and returns a new list
   with the same elements but in ascending order."
  (let [ys (reduce bubble [] xs)]
    (if (= xs ys)
      (apply list xs)
      (recur ys))))

(deftest test-my-sort
  (is (= () (my-sort ())))
  (is (= '(0 1 3 3 4 6 7 8 9) (my-sort '(4 3 6 8 3 0 9 1 7))))
  (is (= '(1 2 3 4 5 6) (my-sort '(1 2 3 4 5 6))))
  (is (= '(1 5 5 5 5 5 5) (my-sort '(5 5 5 1 5 5 5)))))

(defn rotate-left
  "Returns the list that results from rotating lst a total of n elements to the left."
  [n lst]
  (cond (empty? lst) lst
        (> n 0) (nth (iterate #(concat (rest %) (list (first %))) lst) n)
        :else (nth (iterate #(conj (butlast %) (last %)) lst) (* -1 n))))

(deftest test-rotate-left
  (is (= () (rotate-left 5 ())))
  (is (= '(a b c d e f g) (rotate-left 0 '(a b c d e f g))))
  (is (= '(b c d e f g a) (rotate-left 1 '(a b c d e f g))))
  (is (= '(g a b c d e f) (rotate-left -1 '(a b c d e f g))))
  (is (= '(d e f g a b c) (rotate-left 3 '(a b c d e f g))))
  (is (= '(e f g a b c d) (rotate-left -3 '(a b c d e f g))))
  (is (= '(a b c d e f g) (rotate-left 7 '(a b c d e f g))))
  (is (= '(a b c d e f g) (rotate-left -7 '(a b c d e f g))))
  (is (= '(b c d e f g a) (rotate-left 8 '(a b c d e f g))))
  (is (= '(g a b c d e f) (rotate-left -8 '(a b c d e f g))))
  (is (= '(d e f g a b c) (rotate-left 45 '(a b c d e f g))))
  (is (= '(e f g a b c d) (rotate-left -45 '(a b c d e f g)))))

(defn binary
  [n]
  "It returns an empty list. If n is greater than zero, it returns a list with a
   sequence of ones and zeros equivalent to the binary representation of n. "
  (->> [() n]
       (iterate (fn [[r n]]
                  [(cons (rem n 2) r) (quot n 2)]))
       (drop-while (fn [[r n]] (not= n 0)))
       first
       first))

(deftest test-binary
  (is (= () (binary 0)))
  (is (= '(1 1 1 1 0) (binary 30)))
  (is (= '(1 0 1 1 0 0 0 0 0 1 0 0 0 0 1 1) (binary 45123))))

(defn prime-factors
  [n]
  "Takes an integer n as input (assume that n > 0), and returns a list containing
   the prime factors of n in ascending order. The prime factors are the prime
   numbers that divide a number exactly. If you multiply all the prime factors
   you get the original number."
  (loop [lst ()
         i 2
         n n]
    (if (= n 1)
      lst
      (if (= 0 (mod n i))
        (recur (concat lst (list i)) i (/ n i))
        (recur lst (inc i) n)))))

(deftest test-prime-factors
  (is (= () (prime-factors 1)))
  (is (= '(2 3) (prime-factors 6)))
  (is (= '(2 2 2 2 2 3) (prime-factors 96)))
  (is (= '(97) (prime-factors 97)))
  (is (= '(2 3 3 37) (prime-factors 666))))

(defn gcd
  [a b]
  "Takes two positive integer arguments a and b as arguments, where a > 0 and b > 0.
   It returns the greatest common divisor (GCD) of a and b. "
  (last
    (remove
      #(not (and (zero? (mod a %)) (zero? (mod b %))))
      (range 1 (min a b)))))

(deftest test-gcd
  (is (= 1 (gcd 13 7919)))
  (is (= 4 (gcd 20 16)))
  (is (= 6 (gcd 54 24)))
  (is (= 7 (gcd 6307 1995)))
  (is (= 12 (gcd 48 180)))
  (is (= 14 (gcd 42 56))))

(defn insert-everywhere
  [x lst]
  "It returns a new list with all the possible ways in which x can be inserted into every
   position of lst. "
  (map #(concat (take % lst) (cons x (drop % lst))) (range 0 (+ (count lst) 1))))

(deftest test-insert-everywhere
  (is (= '((1)) (insert-everywhere 1 ())))
  (is (= '((1 a) (a 1)) (insert-everywhere 1 '(a))))
  (is (= '((1 a b c) (a 1 b c) (a b 1 c) (a b c 1))
         (insert-everywhere 1 '(a b c))))
  (is (= '((1 a b c d e)
           (a 1 b c d e)
           (a b 1 c d e)
           (a b c 1 d e)
           (a b c d 1 e)
           (a b c d e 1))
         (insert-everywhere 1 '(a b c d e))))
  (is (= '((x 1 2 3 4 5 6 7 8 9 10)
           (1 x 2 3 4 5 6 7 8 9 10)
           (1 2 x 3 4 5 6 7 8 9 10)
           (1 2 3 x 4 5 6 7 8 9 10)
           (1 2 3 4 x 5 6 7 8 9 10)
           (1 2 3 4 5 x 6 7 8 9 10)
           (1 2 3 4 5 6 x 7 8 9 10)
           (1 2 3 4 5 6 7 x 8 9 10)
           (1 2 3 4 5 6 7 8 x 9 10)
           (1 2 3 4 5 6 7 8 9 x 10)
           (1 2 3 4 5 6 7 8 9 10 x))
         (insert-everywhere 'x '(1 2 3 4 5 6 7 8 9 10)))))

(defn deep-reverse
  [lst]
  "It returns a list with the same elements as its input but in reverse order.
   If there are any nested lists, these too should be reversed."
  (reverse (map #(if (list? %) (deep-reverse %) %) lst)))

(deftest test-deep-reverse
  (is (= () (deep-reverse ())))
  (is (= '(3 (d c b) a) (deep-reverse '(a (b c d) 3))))
  (is (= '(((6 5) 4) 3 (2 1))
         (deep-reverse '((1 2) 3 (4 (5 6)))))))

(defn pack
  [lst]
  "Takes a list lst as its argument. If lst contains consecutive repeated elements they
   should be placed in separate sublists."
  (partition-by identity lst))

(deftest test-pack
  (is (= () (pack ())))
  (is (= '((a a a a) (b) (c c) (a a) (d) (e e e e))
         (pack '(a a a a b c c a a d e e e e))))
  (is (= '((1) (2) (3) (4) (5)) (pack '(1 2 3 4 5))))
  (is (= '((9 9 9 9 9 9 9 9 9)) (pack '(9 9 9 9 9 9 9 9 9)))))

(defn compress
  [lst]
  "Takes a list lst as its argument. If lst contains consecutive repeated elements,
   they should be replaced with a single copy of the element."
  (mapcat distinct (pack lst)))

(deftest test-compress
  (is (= () (compress ())))
  (is (= '(a b c d) (compress '(a b c d))))
  (is (= '(a b c a d e)
         (compress '(a a a a b c c a a d e e e e))))
  (is (= '(a) (compress '(a a a a a a a a a a)))))

(defn encode
  [lst]
  "Takes a list lst as its argument. Consecutive duplicates of elements in lst are
   encoded as vectors [n e], where n is the number of duplicates of the element e. "
  (map #(vec (list (count %) (first %))) (pack lst)))

(deftest test-encode
  (is (= () (encode ())))
  (is (= '([4 a] [1 b] [2 c] [2 a] [1 d] [4 e])
         (encode '(a a a a b c c a a d e e e e))))
  (is (= '([1 1] [1 2] [1 3] [1 4] [1 5])
         (encode '(1 2 3 4 5))))
  (is (= '([9 9]) (encode '(9 9 9 9 9 9 9 9 9)))))

(defn encode-modified
  [lst]
  "Takes a list lst as its argument. Consecutive duplicates of elements in lst are
   encoded as vectors [n e], where n is the number of duplicates of the element e.
   But if an element has no duplicates it is simply copied into the result list. "
  (map #(if (= 1 (count %))
          (first %)
          (vec (list (count %) (first %))))
       (pack lst)))

(deftest test-encode-modified
  (is (= () (encode-modified ())))
  (is (= '([4 a] b [2 c] [2 a] d [4 e])
         (encode-modified '(a a a a b c c a a d e e e e))))
  (is (= '(1 2 3 4 5) (encode-modified '(1 2 3 4 5))))
  (is (= '([9 9]) (encode-modified '(9 9 9 9 9 9 9 9 9)))))

(defn decode
  [lst]
  "Takes as its argument an encoded list lst that has the same structure as the
   resulting list from the previous problem. It returns the decoded version of lst."
  (mapcat #(if (vector? %) (repeat (first %) (last %)) (list %)) lst))

(deftest test-decode
  (is (= () (decode ())))
  (is (= '(a a a a b c c a a d e e e e)
         (decode '([4 a] b [2 c] [2 a] d [4 e]))))
  (is (= '(1 2 3 4 5) (decode '(1 2 3 4 5))))
  (is (= '(9 9 9 9 9 9 9 9 9) (decode '([9 9])))))

(run-tests)
