(require '[clojure.string :as str])

(def inputs
  (->> (slurp "input.txt")
       (str/split-lines)))

(defn default [n] (if (nil? n) 0 n))

(defn letter-counts [line]
  (reduce
   (fn [counts letter]
     (let [key (keyword letter)
           count (key counts)]
       (assoc counts
              key (inc (default count)))))
   {} (str/split line #"")))

(defn contains-n? [n count]
  (some #(= % n) (vals count)))

(defn n-count [n counts]
  (reduce
   (fn [total count]
     (if (contains-n? n count)
       (inc total) total))
   0 counts))

; Part 1
(let [count (map letter-counts inputs)]
  (* (n-count 2 count)
     (n-count 3 count)))