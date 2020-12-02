(ns main
  (:require [clojure.string :as str]))

(defn read-input [filename]
  (->> (slurp filename)
       (str/split-lines)
       (map #(Integer/parseInt %))
       (set)))

(defn find-match [target i inputs]
  (let [remainder (- target i)]
    (if (contains? inputs remainder)
      [remainder i] nil)))

;; Part 1
(defn find-matches []
  (let [input (read-input "input.txt")]
   (->> input
        (map #(find-match 2020 % input))
        (remove nil?)
        (distinct)
        (map #(reduce * %)))))

(defn find-match-v2 [i inputs]
  (let [remainder (- 2020 i)]
    (->> inputs
         (map #(find-match remainder % inputs))
         (remove nil?))))

;; Part 2
(defn find-matches-v2 []
  (let [input (read-input "input.txt")]
    (->> input
         (map #(find-match-v2 % input))
         (flatten)
         (distinct)
         (reduce *))))

(comment
  (find-matches)
  (find-matches-v2))