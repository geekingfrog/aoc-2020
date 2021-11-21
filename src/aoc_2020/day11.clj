(ns aoc-2020.day11
  (:require [clojure.java.io]
            [clojure.string]))

(def ^:private example
  "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL")

(defrecord Grid [width height seats])

(defn- parse-input
  ([] (parse-input (slurp (clojure.java.io/resource "day11.txt"))))
  ([raw]
   (let [lines (clojure.string/split-lines raw)
         height (count lines)
         width (count (first lines))]
     (->Grid width height (vec (flatten (map vec lines)))))))

(defn- get-seat [grid x y]
  (when (and (and (>= x 0) (< x (:width grid)))
             (and (>= y 0) (< y (:height grid))))
    (nth (:seats grid) (+ x (* (:width grid) y)))))

(defn- print-grid [grid]
  (doseq [y (range (:height grid))]
    (doseq [x (range (:width grid))]
      (print (get-seat grid x y)))
    (print "\n"))
  grid)

(defn- seat->int [s]
  (case s
    \. 0
    \# 1
    \L 0
    nil 0))

(defn- next-seat [grid i j]
  (let [neighbors
        (apply + (filter
                  some?
                  (for [x [(dec i) i (inc i)]
                        y [(dec j) j (inc j)]
                        :when (or (not= i x) (not= j y))]
                    (seat->int (get-seat grid x y)))))
        seat (get-seat grid i j)]
    (cond
      (and (= \L seat) (= neighbors 0)) \#
      (and (= \# seat) (>= neighbors 4)) \L
      :else seat)))

(defn- next-grid [next-fn grid]
  (let [next-seats
        (vec
         (for [y (range (:height grid))
               x (range (:width grid))]
           (next-fn grid x y)))]
    (assoc grid :seats next-seats)))

(defn- sum-grid [grid]
  (apply + (map seat->int (:seats grid))))

(defn- nl [stuff]
  (print "\n")
  stuff)

;; (print-grid (parse-input example))

(defn- solve-all [next-fn grid]
  (let [grids (iterate (partial next-grid next-fn) grid)
        pairs (map vector (range) grids (rest grids))
        fixed (filter (fn [[_idx a b]] (= a b)) pairs)
        last-grid (last (first fixed))]
    ;; (print-grid last-grid)
    (sum-grid last-grid)))

(defn- solve1 [grid] (solve-all next-seat grid))

(def ^:private example2
  ".......#.
...#.....
.#.......
.........
..#L....#
....#....
.........
#........
...#.....")

(def ^:private example3
  ".............
.L.L.#.#.#.#.
.............")

(defn- next-visible-seat [grid x y [vx vy]]
  (let [coords (rest (iterate (fn [[x y]] [(+ x vx) (+ y vy)]) [x y]))
        seats (drop-while
               #(= \. %)
               (map (fn [[x y]] (get-seat grid x y)) coords))]
    (first seats)))

(defn- next-seat2 [grid x y]
  (let [seat (get-seat grid x y)]
    (if (= \. seat)
      \.
      (let [; clockwise, starts at 12
            directions (for [vx [-1 0 1]
                             vy [-1 0 1]
                             :when (or (not= vx 0) (not= vy 0))]
                         [vx vy])
            neighbors (map #(next-visible-seat grid x y %) directions)
            total (apply + (map seat->int neighbors))]
        (cond
          (and (= \L seat) (= total 0)) \#
          (and (= \# seat) (>= total 5)) \L
          :else seat)))))

(defn- solve2 [grid] (solve-all next-seat2 grid))

(defmulti solve identity)

(defmethod solve 1 [_]
  (solve1 (parse-input)))

(defmethod solve 2 [_]
  (solve2 (parse-input)))
