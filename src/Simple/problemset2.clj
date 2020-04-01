;----------------------------------------------------------
; Problem Set #2
; Date: March 12, 2020.
; Authors:
;          A01377824 Hugo Vazquez Reyes
;          A01371734 Daniela Colin Castañeda
;----------------------------------------------------------

(require '[clojure.test :refer [deftest is run-tests]])

; ------- 1. Replic-----------
(defn replic
  "Returns a new list that replicates n times each element contained in lst"
  [n lst]
  (mapcat #(repeat n %) lst))

; ------- 2. Expand-----------
(defn expand
  "Returns a new list with 1st element repeated once, the 2nd twice and so on"
  [lst]
  (mapcat repeat(range 1 (inc (count lst)))lst))


; ------- 3. Insert-----------
(defn insert
  "Returns a new list with the n in the corresponding place on lst"
  [n lst]
  (sort (cons n lst)))


; ------- 4. My-Sort-----------
(defn my-sort
  "Returns a new sorted list"
  [lst]
  (loop [lst lst
         result ()]
    (if (empty? lst)
      result
      (recur (rest lst) (insert (first lst) result)))))

; ------- 5. Rotate-Left-----------
(defn rotate-left
  "Returns the list that results from rotating lst a total of n elements to the left."
  [n lst]
  (if (empty? lst)
    ()
    (concat (drop (mod n (count lst)) lst) (take (mod n (count lst)) lst))))

; ------- 6. Binary-----------
(defn binary
  "Takes an integer n as input.
  If n is equal to zero, it returns an empty list.
  If n is greater than zero, it returns a list with a sequence of
  ones and zeros equivalent to the binary representation of n. "
  [n]
  (loop [r ()
         n n]
    (if ( zero? n)
      r
      (recur(cons (rem n 2)r)(quot n 2)))))

; ------- 7. Prime-factor-----------
(defn prime-factors
  "Takes an integer n as input and returns a list containing
  the prime factors of n in ascending order."
  [n]
  (loop [lst ()
         i 2
         n n]
    (if (= n 1)
      lst
      (if (= 0 (mod n i))
        (recur (concat lst (list i)) i (/ n i))
        (recur lst (inc i) n)))))

; ------- 8.Gdc-----------
(defn gcd
  "It returns the greatest common divisor (GCD) of a and b"
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))


; ------- 9. Insert-everywhere-----------
(defn insert-everywhere
  "Returns a new list with all the possible ways in which x
  can be inserted into every position of lst"
  [x lst]
  (->> (range (inc (count lst)))
       (map (fn [n] (concat (take n lst)(list x)(drop n lst))))))

; ------- 10. Deep-reverse-----------
(defn deep-reverse
  "Returns a list with the same elements as its input but in reverse order"
  [lst]
  (cond
    (empty? lst) ()
    (list?(first lst))(concat (deep-reverse (rest lst))
                              (list (deep-reverse (first lst))))
    (concat (deep-reverse(rest lst))
            (list (first lst)))))


; ------- 11. Pack-----------
(defn pack
  "Returns a new list with repeated elements packed in a sublist"
  [lst]
  (partition-by identity lst))

; ------- 12. Compress-----------
(defn compress
  "Returns a new list without repeated elements"
  [lst]
  (map (fn [x] (first x)) (pack lst)))

; ------- 13. Encode-----------
(defn encode
  "Returns new list with vectors containing the number of duplicates of the element"
  [lst]
  (->> (pack lst)
       (map (fn [x] [(count x) (first x)]))))

; ------- 14. Encode-modified-----------
(defn encode-modified
  "Returns new list with vectors containing the number of duplicates
  of the element, if there is no duplicates it just concats the element"
  [lst]
  (->>
    (encode lst)
    (map (fn [x]
           (if (= 1 (first x))
             (second x)
             x)))))

; ------- 15. Decode-----------
(defn decode
  "Returns the decoded version of lst"
  [lst]
  (mapcat
    (fn [x]
      (if (vector? x)
        (repeat (first x) (second x))
        (list x)))
    lst))




(deftest test-replic
  (is (= () (replic 7 ())))
  (is (= () (replic 0 '(a b c))))
  (is (= '(a a a) (replic 3 '(a))))
  (is (= '(1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4)
         (replic 4 '(1 2 3 4)))))

(deftest test-expand
  (is (= () (expand ())))
  (is (= '(a) (expand '(a))))
  (is (= '(1 2 2 3 3 3 4 4 4 4) (expand '(1 2 3 4))))
  (is (= '(a b b c c c d d d d e e e e e)
         (expand '(a b c d e)))))

(deftest test-insert
  (is (= '(14) (insert 14 ())))
  (is (= '(4 5 6 7 8) (insert 4 '(5 6 7 8))))
  (is (= '(1 3 5 6 7 9 16) (insert 5 '(1 3 6 7 9 16))))
  (is (= '(1 5 6 10) (insert 10 '(1 5 6)))))

(deftest test-my-sort
  (is (= () (my-sort ())))
  (is (= '(0 1 3 3 4 6 7 8 9) (my-sort '(4 3 6 8 3 0 9 1 7))))
  (is (= '(1 2 3 4 5 6) (my-sort '(1 2 3 4 5 6))))
  (is (= '(1 5 5 5 5 5 5) (my-sort '(5 5 5 1 5 5 5)))))

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

(deftest test-binary
  (is (= () (binary 0)))
  (is (= '(1 1 1 1 0) (binary 30)))
  (is (= '(1 0 1 1 0 0 0 0 0 1 0 0 0 0 1 1) (binary 45123))))

(deftest test-prime-factors
  (is (= () (prime-factors 1)))
  (is (= '(2 3) (prime-factors 6)))
  (is (= '(2 2 2 2 2 3) (prime-factors 96)))
  (is (= '(97) (prime-factors 97)))
  (is (= '(2 3 3 37) (prime-factors 666))))

(deftest test-gcd
  (is (= 1 (gcd 13 7919)))
  (is (= 4 (gcd 20 16)))
  (is (= 6 (gcd 54 24)))
  (is (= 7 (gcd 6307 1995)))
  (is (= 12 (gcd 48 180)))
  (is (= 14 (gcd 42 56))))

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

(deftest test-deep-reverse
  (is (= () (deep-reverse ())))
  (is (= '(3 (d c b) a) (deep-reverse '(a (b c d) 3))))
  (is (= '(((6 5) 4) 3 (2 1))
         (deep-reverse '((1 2) 3 (4 (5 6)))))))

(deftest test-pack
  (is (= () (pack ())))
  (is (= '((a a a a) (b) (c c) (a a) (d) (e e e e))
         (pack '(a a a a b c c a a d e e e e))))
  (is (= '((1) (2) (3) (4) (5)) (pack '(1 2 3 4 5))))
  (is (= '((9 9 9 9 9 9 9 9 9)) (pack '(9 9 9 9 9 9 9 9 9)))))

(deftest test-compress
  (is (= () (compress ())))
  (is (= '(a b c d) (compress '(a b c d))))
  (is (= '(a b c a d e)
         (compress '(a a a a b c c a a d e e e e))))
  (is (= '(a) (compress '(a a a a a a a a a a)))))

(deftest test-encode
  (is (= () (encode ())))
  (is (= '([4 a] [1 b] [2 c] [2 a] [1 d] [4 e])
         (encode '(a a a a b c c a a d e e e e))))
  (is (= '([1 1] [1 2] [1 3] [1 4] [1 5])
         (encode '(1 2 3 4 5))))
  (is (= '([9 9]) (encode '(9 9 9 9 9 9 9 9 9)))))

(deftest test-encode-modified
  (is (= () (encode-modified ())))
  (is (= '([4 a] b [2 c] [2 a] d [4 e])
         (encode-modified '(a a a a b c c a a d e e e e))))
  (is (= '(1 2 3 4 5) (encode-modified '(1 2 3 4 5))))
  (is (= '([9 9]) (encode-modified '(9 9 9 9 9 9 9 9 9)))))

(deftest test-decode
  (is (= () (decode ())))
  (is (= '(a a a a b c c a a d e e e e)
         (decode '([4 a] b [2 c] [2 a] d [4 e]))))
  (is (= '(1 2 3 4 5) (decode '(1 2 3 4 5))))
  (is (= '(9 9 9 9 9 9 9 9 9) (decode '([9 9])))))
