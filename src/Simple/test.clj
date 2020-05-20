;"Returns an infinite lazy sequence of integers (most of
;  them BigInts) starting with n, then n concatenated twice,
;  then n concatenated three times, and so on, to infinity
;  and beyond."

(use 'clojure.math.numeric-tower)

(defn concat-repeat-num
  "Returns an infinite lazy sequence of integers (most of
  them BigInts) starting with n, then n concatenated twice,
  then n concatenated three times, and so on, to infinity
  and beyond."
  [n]
  (iterate #(+' (*' (expt 10 (count (str %))) %) %) n)
  )

(defn fi [x]
  (lazy-seq
    (cons x (fi (+' (*' (expt 10 (count (str x))) x) x)))
    )
  )


