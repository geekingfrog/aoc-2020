(ns aoc-2020.day09
  (:require [clojure.java.io]
            [clojure.string]))

(def example
  "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576")

(defn- parse-input
  ([] (parse-input (slurp (clojure.java.io/resource "day09.txt"))))
  ([raw]
   (->> raw
        (clojure.string/split-lines)
        (map #(BigInteger. %))
        (vec))))

(defn- is-sum? [nums target]
  (some?
   (some identity (for [i (range (count nums))
                        j (range (inc i) (count nums))]
                    (= target (+ (nth nums i) (nth nums j)))))))

(defn- valid-xmas?
  ([digits idx] (valid-xmas? digits idx 25))
  ([digits idx span]
   (cond
     (< idx span) true
     :else (is-sum? (subvec digits (- idx span) idx) (nth digits idx)))))

(defn- all-ranges [nums]
  (let [n (count nums)]
    (sort-by
     count
     (filter
      identity
      (for [start (range (inc n))
            l (range 2 (inc n))]
        (let [end (+ start l)]
          (when (<= end n)
            (subvec nums start end))))))))

(defn- weak-num
  ([nums] (weak-num nums 25))
  ([nums span]
   (let [idxs (filter #(not (valid-xmas? nums % span)) (range (count nums)))]
     (nth nums (first idxs)))))

(defn- encryption-weakness [nums target]
  (let [weak-range (first (filter #(= target (apply + %)) (all-ranges nums)))
        smol (apply min weak-range)
        big (apply max weak-range)]
    (+ smol big)))

(encryption-weakness (parse-input example) 127)

(defmulti solve identity)

(defmethod solve 1 [_]
  (weak-num (parse-input)))

(defmethod solve 2 [_]
  (let [nums (parse-input)
        target (solve 1)]
    (encryption-weakness nums target)))

;; 76096372N
