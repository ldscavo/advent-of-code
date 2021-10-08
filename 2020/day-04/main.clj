(ns main
  (:require [clojure.string :as str]))

(defn read-input [file]
  (as-> (slurp file) input
    (str/split input #"\r\n\r\n")))

;; Part 1 filter
(defn has-all-fields? [input]
  (->> ["ecl:" "pid:" "eyr:" "hcl:" "byr:" "iyr:" "hgt:"]
       (map #(str/includes? input %))
       (reduce #(and %1 %2))))

;; Part 2 filter
(defn fit-req-params [input]
  (->> [#"byr:[1920-2002]"]
       (map #(re-matches % input))
       (reduce #(and %1 %2))))

(defn get-valid-passports [f]
  (->> (read-input "input.txt")
       (map f)
       (filter true?)
       (count)))

;; Part 1
(get-valid-passports has-all-fields?)