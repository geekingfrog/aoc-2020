(ns aoc-2020.day04
  (:require [clojure.string :as str]
            [clojure.set]
            [clojure.java.io]))

(defn- mk-kv [word]
  (let [[k v] (str/split word #":")]
    [(keyword k) v]))

(defn- parse-passport [raw]
  (let [words (str/split raw #"(\n|\s+)")
        parsed (into {} (map mk-kv words))]
    parsed))

(defn- parse-input []
  (let [raw (slurp (clojure.java.io/resource "day04.txt"))
        passports (str/split raw #"\n\n")
        parsed (map parse-passport passports)]
    parsed))

(def ^:private required-keys #{:byr :iyr :eyr :hgt :hcl :ecl :pid})

(defn- valid-passport? [passport]
  (= required-keys
     (clojure.set/intersection required-keys
                               (set (keys passport)))))

(def test-passport {:byr 2000 :iyr 2 :eyr 3 :hgt 4 :hcl 5 :ecl 6 :pid 7 :cid 8})
(valid-passport? test-passport)

(defn- parse-int [x]
  (when (some? x) (Integer/parseInt x)))

(defmulti validate-rule (fn [x _] x))

(defmethod validate-rule :byr [rule passport]
  (when-let [v (parse-int (rule passport))]
    (and (some? v) (>= v 1920) (<= v 2002))))

(defmethod validate-rule :iyr [rule passport]
  (let [v (parse-int (rule passport))]
    (and (some? v) (>= v 2010) (<= v 2020))))

(defmethod validate-rule :eyr [rule passport]
  (let [v (parse-int (rule passport))]
    (and (some? v) (>= v 2020) (<= v 2030))))

(defmethod validate-rule :hgt [rule passport]
  (let [v (rule passport)]
    (if (nil? v)
      false
      (if-let [[_ height unit] (re-matches #"(\d+)(in|cm)" v)]
        (let [height (Integer/parseInt height)]
          (case unit
            "cm" (and (>= height 150) (<= height 193))
            "in" (and (>= height 59) (<= height 76))
            false))
        false))))

(defmethod validate-rule :hcl [rule passport]
  (let [v (rule passport)]
    (and (some? v) (some? (re-matches #"#[0-9,a-f]{6}" v)))))

(defmethod validate-rule :ecl [rule passport]
  (let [v (rule passport)]
    (and (some? v) (some? (re-matches #"(amb|blu|brn|gry|grn|hzl|oth)" v)))))

(defmethod validate-rule :pid [rule passport]
  (let [v (rule passport)]
    (and (some? v) (some? (re-matches #"[0-9]{9}" v)))))

(defn- valid-with-rules? [passport]
  (every? #(validate-rule % passport) required-keys))

(defmulti solve identity)

(defmethod solve 1 [_]
  (println
   (count
    (filter valid-passport? (parse-input)))))

(defmethod solve 2 [_]
  (println
   (count
    (filter valid-with-rules? (parse-input)))))
