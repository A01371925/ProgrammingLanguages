;==========================================================
; A01378843 Sonia Leilani Ramos Núñez
; A01371925 Luis Eduardo Díaz Niño de Rivera
;==========================================================

(defn head [at t]
  (doseq [i (range t)]
    (doseq [j at]
      (print '*))
    (println)))

(defn body [at s t n]
  (doseq [w (range n)]
    (doseq [i (range s)]
      (doseq [j (range n)]
        (doseq [u (range t)]
          (print '*))
        (doseq [k (range s)]
          (print '.)
          ))
      (doseq [w (range t)]
        (print '*))
      (println)
      )
    (head at t)
    )
  )

(defn grid [s t n]
  (let [at (range (+ (+ t (* n (+ s t)))))]
    (head at t)
    (body at s t n)
    )
  )

(grid 3 1 5)
(grid 1 1 1)
(grid 3 2 3)
(grid 4 3 2)
(grid 3 2 5)
