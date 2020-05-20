;==========================================================
; A01378843 Sonia Leilani Ramos Núñez
; A01371925 Luis Eduardo Díaz Niño de Rivera
;==========================================================

(defn conway-aux
  [x]
  (lazy-seq
    (cons x (conway-aux
              (flatten
                (mapcat (fn [e]
                          (list (count e) (first e))
                          ) (partition-by identity x)))))))

(defn conway []
  (conway-aux '(1)))

(def c (conway))
(first c)
(take 8 c)
(take 10 c)
(nth c 12)