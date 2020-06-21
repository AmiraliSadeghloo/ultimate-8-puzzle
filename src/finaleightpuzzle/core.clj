(ns finaleightpuzzle.core
  (:use [finaleightpuzzle.node]
        [finaleightpuzzle.vector])
  (:gen-class))
;;///////////////////////////
(defn solvable? [x]
  (let [flattened-removed-empty (into [] (filter #(not= :empty %) (into [] (flatten x))))]
    (loop [counts [] xs flattened-removed-empty]
      (if (seq xs)
        (let [head (first xs)
              tail (rest xs)
              counter (fn [xi xj] (if (> xj head) (inc xi) xi))]
          (recur (conj counts (reduce counter
                                      0
                                      tail)) tail))
        (even? (reduce + counts))))))

;;//////////////////////////
(defn main [init]                                           ;;main
  (if (solvable? init)
    (loop [unchecked (conj [] (mapped-input init)) checked []]
      (let [curr (find-curr unchecked)]
        (if (= 0 (h-cost (:vector curr)))
          (:history curr)
          (do
            ;(println "next curr: " curr)
            ;(println "unchecked: " unchecked)
            ;(println "checked: " checked)
            ;(println "removed unchecked" (remove-current unchecked curr) )
            ;(println "moves" (movements curr))
            ;(println "next unchecked: " (moves-to-unchecked (remove-current unchecked curr) (movements curr)))
            ;(println "==========================")
            (recur (moves-to-unchecked (remove-current unchecked curr) (movements curr))
                   (conj checked curr))))))
    (throw (Exception. "not solvable input"))))

;;///////////////////////////
;(main [[1 2 3] [4 5 6] [:empty 7 8]])

;(main [[2 3 4]
;       [1 5 :empty]
;       [7 6 8]])


