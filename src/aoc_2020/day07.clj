(ns aoc-2020.day07
  (:require [clojure.java.io]
            [clojure.string :as str]
            [clojure.set]))

(defn- parse-line [raw-line]
  (let [[_ bag inner-raw] (re-matches #"(\w+ \w+) bags contain (.*)\." raw-line)
        inner (map rest (re-seq #"(?:, )?(\d+) (\w+ \w+) bag" inner-raw))
        parsed-inner (map (fn [[c b]] [(Integer/parseInt c) b]) inner)]
    [bag parsed-inner]))

(defn- parse-input
  ([] (parse-input (slurp (clojure.java.io/resource "day07.txt"))))
  ([raw]
   (let [parsed-lines (map parse-line (str/split-lines raw))]
     (into {} parsed-lines))))

(defn- reverse-map [bags]
  (apply merge-with
         concat
         (map
          (fn [[k v]]
            (reduce (fn [acc [_num b]] (assoc acc b [k])) {} v))
          bags)))

(defn can-contain
  ([contain-map] (can-contain contain-map "shiny gold"))
  ([contain-map seed]
   (let [direct (set (get contain-map seed))
         indirect (apply clojure.set/union (map #(can-contain contain-map %) direct))]
     (clojure.set/union direct indirect))))

(defn count-bag
  "Total number of bag (including the given one)"
  ([bag-map bag] (count-bag bag-map bag (transient {})))
  ([bag-map bag cache]
   (if-let [v (get bag cache)]
     v
     (let [inner (get bag-map bag)
           new-val (inc (apply +
                               (map (fn [[c b]]
                                      (* c (count-bag bag-map b))) inner)))]
       (assoc! cache bag new-val)
       new-val))))

;; (def ^:private example
;;   "light red bags contain 1 bright white bag, 2 muted yellow bags.
;; dark orange bags contain 3 bright white bags, 4 muted yellow bags.
;; bright white bags contain 1 shiny gold bag.
;; muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
;; shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
;; dark olive bags contain 3 faded blue bags, 4 dotted black bags.
;; vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
;; faded blue bags contain no other bags.
;; dotted black bags contain no other bags.")
;;
;; (def ^:private example2
;;   "shiny gold bags contain 2 dark red bags.
;; dark red bags contain 2 dark orange bags.
;; dark orange bags contain 2 dark yellow bags.
;; dark yellow bags contain 2 dark green bags.
;; dark green bags contain 2 dark blue bags.
;; dark blue bags contain 2 dark violet bags.
;; dark violet bags contain no other bags.")
;;
;; (count-bag (parse-input example) "shiny gold")
;; (count-bag (parse-input example2) "shiny gold")
;;
;; (count (can-contain (reverse-map (parse-input example))))

(defmulti solve identity)

(defmethod solve 1 [_]
  (count
   (can-contain
    (reverse-map (parse-input)))))

(defmethod solve 2 [_]
  (dec (count-bag (parse-input) "shiny gold")))
