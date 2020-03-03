(require '[clojure.test :refer [deftest is run-tests]])

(defn !
     "Computes the factorial of n"
     [n]
     (if (zero? n)
       1
       (*' n (! (dec n)))))

(defn !!
  [n]
  (loop [i 1
         acum 1]
    (if (> i n)
      acum
    (recur (inc i) (*' acum i)))))

(defn !!!
  [n]
  (reduce *' (range 1 (inc n))))

(defn duplicate
  [lst]
  (if (empty? lst)
    ()
    (cons
      (first lst)
      (cons
        (first lst)
        (duplicate (rest lst))))))

(defn duplicateLoop
  [lst]
  (loop [lst lst
         acum ()]
    (if (empty? lst)
      (reverse acum)
      (recur
        (rest lst)
        (cons
          (first lst)
          (cons
            (first lst)
            acum))))))

(defn duplicateSequence
  [lst]
  (mapcat #(list % %) lst))

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



(run-tests)