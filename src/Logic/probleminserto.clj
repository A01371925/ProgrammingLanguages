(require '[clojure.core.logic :as logic])
(require '[clojure.core.logic.fd :as fd])

(logic/defne inserto
             [x lst result]
             ([x []  [x]])
             ([x [head . tail] [x head . tail]]
              (fd/<= x head))
             ([x [ head . tail] result]
              (logic/fresh [temp]
                           (inserto x tail temp)
                           (fd/> x head)
                           (logic/conso head temp result))))


(logic/run 1 [q] (inserto 5 [1 2 3 4 6 7 8] q))
(logic/run 1 [q] (inserto 5 [] q))
(logic/run 3 [p q] (inserto p q [1 2 3]))
