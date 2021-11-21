(ns aoc-2020.day05
  (:require [clojure.java.io]
            [clojure.set]
            [clojure.string :as str]))

(defn- chr->bin [c]
  (case c
    \F 0
    \B 1
    \R 1
    \L 0))

(defn- decode-bin [bits]
  (reduce #(+ %2 (bit-shift-left %1 1)) 0 bits))

(defn- decode [raw-str]
  (let [[row col] (split-at 7 raw-str)]
    {:row (decode-bin (map chr->bin row))
     :col (decode-bin (map chr->bin col))}))

(defn- seat->id [{:keys [row col]}]
  (+ (* 8 row) col))

(defn parse-input []
  (let [raw (slurp (clojure.java.io/resource "day05.txt"))
        raw-seats (str/split-lines raw)
        seats (map decode raw-seats)]
    seats))

(defmulti solve identity)

(defmethod solve 1 [_]
  (apply max (map seat->id (parse-input))))

(defmethod solve 2 [_]
  (let [seats (sort (map seat->id (parse-input)))
        pairs (map vector seats (rest seats))
        missing (filter (fn [[a b]] (> (- b a) 1)) pairs)]
    (inc (first (first missing)))))
