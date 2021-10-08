(ns main
  (:require [clojure.string :as str]))

(defn read-input [file]
  (->> (slurp file)
       (str/split-lines)
       (map #(str/split % #""))))

(defn get-next [i n length]
  (mod (+ i n) length))

(defn find-trees [input line]
  (let [count (:count input)]
    {:i (get-next (:i input) (:n input) (:length input))
     :n (:n input)
     :count (if (= (nth line (:i input)) "#") (+ count 1) count)
     :length (:length input)}))

(defn every-n [n input]
  (->> input
       (partition n)
       (apply map list)
       (first)))

(defn get-all [[x y] input]
  (->> input
       (every-n y)
       (reduce find-trees
               {:i 0 :n x :count 0 :length (count (nth input 0))})))

;; Part 1
(let [input (read-input "input.txt")]
  (->> input
       (get-all [3 1])
       (:count)))

;; Part 2
(let [input (read-input "input.txt")
      slopes [[1 1] [3 1] [5 1] [7 1] [1 2]]]  
  (->> slopes
       (map #(get-all % input))
       (map :count)
       (reduce *)))