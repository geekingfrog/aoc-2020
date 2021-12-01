(ns aoc-2020.core
  (:require [aoc-2020.day04 :as day04]
            [aoc-2020.day05 :as day05]
            [aoc-2020.day07 :as day07]
            [aoc-2020.day08 :as day08]
            [aoc-2020.day09 :as day09]
            [aoc-2020.day10 :as day10]
            [aoc-2020.day11 :as day11]
            [aoc-2020.day12 :as day12])
  (:gen-class :main true)
  ;; (:require [cheshire.core :as json]
  ;;           [rules-clj.inference :refer :all])
  )

(defn -main
  [& [day part]]
  (let [day (Integer/parseInt day)
        part (Integer/parseInt part)]
    (println
     (condp = day
       4 (day04/solve part)
       5 (day05/solve part)
       7 (day07/solve part)
       8 (day08/solve part)
       9 (day09/solve part)
       10 (day10/solve part)
       11 (day11/solve part)
       12 (day12/solve part)
       (str "not yet solved for" day part)))))
