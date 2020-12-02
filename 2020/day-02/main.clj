(ns main
  (:require [clojure.string :as str]))

(defn to-char [ch]
  (-> ch char-array first))

(defn parse-rules [rulestr]
  (let [split-rules (str/split rulestr #" ")]
    {:range (-> split-rules
                (first)
                (str/split #"-")
                (->> (map #(Integer/parseInt %))))
     :char (-> split-rules second to-char)}))

(defn read-input [filename]
  (->> (slurp filename)
       (str/split-lines)
       (map #(str/split % #": "))
       (map (fn [i] [(parse-rules (first i)) (second i)]))))

(defn char-count [value ch]
  (or ((frequencies value) ch) 0))

(defn get-valid-password-count [f]
  (->> "input.txt"
       (read-input)
       (map #(f (first %) (second %)))
       (remove false?)
       (count)))

(defn xor [x y]
  (and (or x y) (or (not x) (not y))))

;; Part 1
(defn valid? [rules password]
  (let [count (char-count password (:char rules))]
   (and (<= (-> rules :range first) count)
        (>= (-> rules :range second) count))))

;; Part 2
(defn valid-v2? [rules password]
  (let [fst (-> rules :range first)
        snd (-> rules :range second)
        pswd-array (-> password char-array vec)]
    (xor (= (:char rules) (nth pswd-array (- fst 1)))
        (= (:char rules) (nth pswd-array (- snd 1))))))

(comment
  (get-valid-password-count valid?)
  (get-valid-password-count valid-v2?))