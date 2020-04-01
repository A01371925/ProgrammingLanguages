;==========================================================
; A01371925 Luis Eduardo Díaz Niño de Rivera
;==========================================================

(require '[clojure.test :refer [deftest is run-tests]])

;==========================================================

(defn lucky-number?
  "Returns true if n is a positive integer number that is
  exactly divisible by 7 or contains at least one digit 7.
  Otherwise returns false."
  [n]
  (if (= (mod n 7) 0)
    true
    (= (some #{7} (->> n
                       (iterate #(quot % 10))
                       (map #(mod % 10))))
       7)))



;==========================================================
(deftest test-lucky-number?
  (is (lucky-number? 7))
  (is (lucky-number? 2828))
  (is (lucky-number? 773704))
  (is (lucky-number? 0))
  (is (lucky-number? 14890123808))
  (is (lucky-number? 100000700000))
  (is (not (lucky-number? 1)))
  (is (not (lucky-number? 26)))
  (is (not (lucky-number? 123456890)))
  (is (not (lucky-number? 14890123806))))

;==========================================================
(run-tests)
