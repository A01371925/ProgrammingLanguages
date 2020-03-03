(require '[clojure.test :refer [deftest is run-tests]])
(require '[clojure.math.numeric-tower :refer [sqrt]])

(defn f2c
  "Converts n degrees Fahrenheit into
  degrees Celsius "
  [f]
  (/ (* 5 (- f 32)) 9.0))

(defn sign
  "Evaluates the sing of n"
  [n]
  (if (< n 0)
    -1
    (if (> n 0)
      1
      0)))
(defn roots
  "Returns a vector containing the two possible roots that solve a quadratic equation"
  [a,b,c]
  (/
    (+ (- b) (sqrt ( - (* b b) (* 4 a c)))) (* 2 a)))
(defn rootsp
  [a,b,c]
  (let [d (- b)
        e (sqt (- (* b b) (* 4 a c)))
        f (* 2 a)]
    (/ (+ d e))))


(defn bmi
  [w,h]
  (let [
        BMI (/ w h)]
    (if (< 20 BMI)
      'underweight
      (if (< BMI 25)
        'normal
        (if (< BMI 30)
          'obese1
          (if (< BMI 40)
            'obese2
            'obese3))))))


(deftest test-f2c
  (is (= 100.0 (f2c 212.0)))
  (is (= 0.0 (f2c 32.0)))
  (is (= -40.0 (f2c -40.0))))

(deftest test-sign
  (is (= -1 (sign -5)))
  (is (= 1 (sign 10)))
  (is (= 0 (sign 0))))
(deftest test-roots
  (is (= [-1 -1] (roots 2 4 2)))
  (is (= [0 0] (roots 1 0 0)))
  (is (= [-1/4 -1] (roots 4 5 1))))
(deftest test-bmi
  (is (= 'underweight (bmi 45 1.7)))
  (is (= 'normal (bmi 55 1.5)))
  (is (= 'obese1 (bmi 76 1.7)))
  (is (= 'obese2 (bmi 81 1.6)))
  (is (= 'obese3 (bmi 120 1.6))))
(run-tests)


