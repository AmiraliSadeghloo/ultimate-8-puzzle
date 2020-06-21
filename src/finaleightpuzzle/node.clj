(ns finaleightpuzzle.node)

(defn find-curr [unchecked]                                 ; A* -> f(x) = g(x) + h(x)
  (reduce (fn [min-map each]
            (if (= (+ (count (:history min-map)) (:h min-map))
                   (+ (count (:history each)) (:h each)))
              (if (< (:h min-map) (:h each))
                min-map
                each)
              (if (< (+ (count (:history min-map)) (:h min-map))
                     (+ (count (:history each)) (:h each)))
                min-map
                each)))
          (first unchecked) (rest unchecked)))
;;/////////////////////
(defn moves-to-unchecked [unchecked movements]              ;this will recur to unchecked each time
  (let [legal-moves (filter #(not= false %) movements)]
    (reduce (fn [a b] (conj a b)) unchecked legal-moves)))
;(moves-to-unchecked [{:a 1} {:b 2}] [{:c 3} false {:d 4}])
;;/////////////////////
(defn remove-current [unchecked curr]
  (into [] (remove #(= curr %) unchecked)))
;;/////////////////////

