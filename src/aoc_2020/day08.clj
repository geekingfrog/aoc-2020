(ns aoc-2020.day08
  (:require [clojure.java.io]
            [clojure.string]))

(defn- parse-line [l]
  (let [[_ ins v] (re-matches #"(\w+)\s+((?:-|\+)\d+)" l)]
    [(keyword ins) (Integer/parseInt v)]))

(defn- mk-cpu [prog]
  {::ic 0
   ::acc 0
   ::program prog})

(defn- parse-program
  ([] (parse-program (slurp (clojure.java.io/resource "day08.txt"))))
  ([raw]
   (mk-cpu (vec (map parse-line (clojure.string/split-lines raw))))))

(defn- get-ins [{:keys [::program ::ic]}]
  (if (>= ic (count program))
    nil
    (nth program ic)))

(def example
  "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")

(defn- interpret-once [{:keys [::program ::acc] :as state}]
  (if-let [[ins v] (get-ins state)]
    (case ins
      :nop (update state ::ic inc)
      :acc (-> state
               (assoc ::acc (+ v acc))
               (update ::ic inc))
      :jmp (update state ::ic #(+ v %)))
    state))

(defn- terminated? [cpu] (nil? (get-ins cpu)))

(defn- break-before-loop [cpu]
  (loop [cpu cpu
         seen #{}]
    (let [next-state (interpret-once cpu)]
      (if (contains? seen (::ic next-state))
        cpu
        (recur next-state (conj seen (::ic cpu)))))))

(defn- mutate-prog
  ([prog] (mutate-prog prog 0))
  ([prog idx]
   (if (>= idx (count prog))
     nil
     (let [[ins v] (nth prog idx)]
       (case ins
         :jmp (lazy-seq (cons prog (cons (assoc prog idx [:nop v]) (mutate-prog prog (inc idx)))))
         :nop (lazy-seq (cons prog (cons (assoc prog idx [:jmp v]) (mutate-prog prog (inc idx)))))
         (lazy-seq (mutate-prog prog (inc idx))))))))

(defn- fix-and-run [cpu]
  (let [all-cpus (map mk-cpu (mutate-prog (::program cpu)))]
    (->> all-cpus
         (map #((juxt ::acc terminated?) (break-before-loop %)))
         (filter last)
         first
         first)))

(defmulti solve identity)

(defmethod solve 1 [_]
  (::acc (break-before-loop (parse-program))))

(defmethod solve 2 [_]
  (fix-and-run (parse-program)))
