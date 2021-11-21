(ns aoc-2020.day10
  (:require [clojure.java.io]
            [clojure.string]))

(defn- parse-input
  ([] (parse-input (clojure.java.io/resource "day10.txt")))
  ([raw] (map #(Integer/parseInt %) (clojure.string/split-lines (slurp raw)))))

(def ^:private example1
  [16 10 15 5 1 11 7 19 6 12 4])
(def ^:private example2
  [28 33 18 42 31 14 46 20 48 47 24 23 49 45 19 38 39 11 1 32 25 35 8 17 7 9 4 2 34 10 3])

(defn- solve1 [adaptors]
  (let [adaptors (into [0] (sort adaptors))
        target (+ 3 (last adaptors))
        adaptors (conj adaptors target)
        pairs (map vector adaptors (rest adaptors))
        diff1 (count (filter (fn [[a b]] (= 1 (- b a))) pairs))
        diff3 (count (filter (fn [[a b]] (= 3 (- b a))) pairs))]
    (* diff1 diff3)))

(defn- num-combo [adaptors table start target]
  (cond
    (< start 0) 0
    (contains? adaptors target) (nth table start)
    :else 0))

(defn- solve2
  ([adaptors] (solve2 adaptors (apply max adaptors)))
  ([adaptors target]
   (loop [table (transient (assoc (vec (repeat (inc (+ 3 (apply max adaptors))) 0))
                                  0
                                  1))
          adaptors (set adaptors)
          i 1]
     (if (> i target)
       (let [v (nth table target)] v)
       (let [a1 (num-combo adaptors table (- i 1) i)
             a2 (num-combo adaptors table (- i 2) i)
             a3 (num-combo adaptors table (- i 3) i)
             v (+ a1 a2 a3)]
         (assoc! table i v)
         (recur table adaptors (inc i)))))))

(defmulti solve identity)

(defmethod solve 1 [_]
  (solve1 (parse-input)))

(defmethod solve 2 [_]
  (solve2 (parse-input)))
