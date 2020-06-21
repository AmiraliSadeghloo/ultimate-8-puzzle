(ns finaleightpuzzle.vector)

(def goal [[1 2 3] [4 5 6] [7 8 :empty]])
;;//////////////////
(defn swap-right
  [x [i1 j1]]
  (let [f (get-in x [i1 j1])
        q (assoc-in x [i1 j1] (get-in x [i1 (+ 1 j1)]))
        s (assoc-in q [i1 (+ 1 j1)] f)]
    s))

(defn swap-left
  [x [i1 j1]]
  (let [f (get-in x [i1 j1])
        q (assoc-in x [i1 j1] (get-in x [i1 (- j1 1)]))
        s (assoc-in q [i1 (- j1 1)] f)]
    s))

(defn swap-down
  [x [i1 j1]]
  (let [f (get-in x [i1 j1])
        q (assoc-in x [i1 j1] (get-in x [(+ i1 1) j1]))
        s (assoc-in q [(+ 1 i1) j1] f)]
    s))

(defn swap-up
  [x [i1 j1]]
  (let [f (get-in x [i1 j1])
        q (assoc-in x [i1 j1] (get-in x [(- i1 1) j1]))
        s (assoc-in q [(- i1 1) j1] f)]
    s))
;;/////////////////
(defn zero-finder
  [input]
  {:zero (first (for [i (range 3)
                      j (range 3)
                      :when (= :empty (get-in input [i j]))]
                  [i j]
                  ))})
;;///////////////
(defn h-cost [v]                                            ; counts input's different elements to goal
  (let [v-flat (into [] (flatten v))
        goal-flat (into [] (flatten goal))]
    (- (count goal-flat) (loop [counted 0 [next1 & rest1] v-flat [next2 & rest2] goal-flat]
                           (if-not (and next1 next2)
                             counted
                             (if (= next2 next1)
                               (recur (inc counted) rest1 rest2)
                               (recur counted rest1 rest2)))))))
;(h-cost [[1 2 3] [4 5 6] [7 0 8]])
;/////////////////
(defn mapped-input [init]                                   ;;maps initial value given
  {:vector init :history [] :h (h-cost init)})
;;///////////////////
(defn move-reversed [move]                                  ;;can't go back in tree
  (case move
    :up :down
    :right :left
    :left :right
    :down :up
    nil))
;;///////////////////
(defn movements                                             ;; creates all 4 directions.
  [current]                                                 ;; checked for legal moves in moves-to-unchecked
  (let [zero (:zero (zero-finder (:vector current)))]
    [(if (or (= :right (move-reversed (last (:history current)))) (= 2 (second zero)))
       false
       {:vector  (swap-right (:vector current) zero)
        :history (conj (:history current) :right)
        :h       (h-cost (swap-right (:vector current) zero))})

     (if (or (= :left (move-reversed (last (:history current)))) (= 0 (second zero)))
       false
       {:vector  (swap-left (:vector current) zero)
        :history (conj (:history current) :left)
        :h       (h-cost (swap-left (:vector current) zero))})

     (if (or (= :down (move-reversed (last (:history current)))) (= 2 (first zero)))
       false
       {:vector  (swap-down (:vector current) zero)
        :history (conj (:history current) :down)
        :h       (h-cost (swap-down (:vector current) zero))})

     (if (or (= :up (move-reversed (last (:history current)))) (= 0 (first zero)))
       false
       {:vector  (swap-up (:vector current) zero)
        :history (conj (:history current) :up)
        :h       (h-cost (swap-up (:vector current) zero))})]))
;;///////////////////

