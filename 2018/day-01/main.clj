(require '[clojure.string :as str])

(defn slurp->ints [filename]
  (->> (slurp filename)
       (str/split-lines)
       (map #(Integer/parseInt %))))

(def inputs
  (slurp->ints "input.txt"))

; Part 1
(println (reduce + inputs))

; Part 2
(loop [numbers (cycle inputs)
       sum 0
       existing #{}]
  (let [num (+ sum (first numbers))]
   (if (contains? existing num)
     (println num)
     (recur (rest numbers) num (conj existing num)))))