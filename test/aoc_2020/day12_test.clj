(ns aoc-2020.day12-test
  (:require [clojure.test :refer :all]
            [aoc-2020.day12 :as day12]))

(deftest move
  (is (= 2 (-> day12/init-ship
               (day12/move-ship [\F 1])
               (day12/move-ship [\L 90])
               (day12/move-ship [\F 1])
               (day12/distance))))

  (is (= 2 (-> day12/init-ship
               (day12/move-ship [\F 1])
               (day12/move-ship [\R 90])
               (day12/move-ship [\F 1])
               (day12/distance))))

  (is (= 3 (-> day12/init-ship
               (day12/move-ship [\F 1])
               (day12/move-ship [\S 1])
               (day12/move-ship [\F 1])
               (day12/distance))))

  (is (= 0 (-> day12/init-ship
               (day12/move-ship [\F 1])
               (day12/move-ship [\R 180])
               (day12/move-ship [\F 1])
               (day12/distance))))

  (is (= 0 (-> day12/init-ship
               (day12/move-ship [\W 1])
               (day12/move-ship [\F 1])
               (day12/distance)))))

(deftest move-waypoint
  (is (= 286
         (day12/distance
          (reduce day12/move-waypoint day12/init-ship
                  [[\F 10]
                   [\N 3]
                   [\F 7]
                   [\R 90]
                   [\F 11]])))))
