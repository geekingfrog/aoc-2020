(ns aoc-2020.day12
  (:require [clojure.java.io]
            [clojure.string]
            ;; [clojure.java.math :refer [cos sin]]
            ))

(defn- parse-line [l]
  (let [[_ d n] (re-matches #"(\w)(\d+)" l)]
    [(first d) (Integer/parseInt n)]))

(defn- ->dirvec
  "not really radiant, since we're moving on a 2D grid
  instead of using cos/sin, stay with integer.

  y
  ^
  |
  |
  |
  +---------> x
  "
  [deg]
  (case deg
    0 [1 0]
    90 [0 1]
    180 [-1 0]
    270 [0 -1]
    -90 [0 -1]
    -180 [-1 0]
    -270 [0 1]))

(defn- parse-input
  ([] (parse-input (slurp (clojure.java.io/resource "day12.txt"))))
  ([raw] (map parse-line (clojure.string/split-lines raw))))

(def example
  "F10
N3
F7
R90
F11")

(defn move-ship [{:keys [x y dirx diry] :as ship} [action n]]
  (condp = action
    \N (assoc ship :y (+ y n))
    \S (assoc ship :y (- y n))
    \E (assoc ship :x (+ x n))
    \W (assoc ship :x (- x n))
    \L (let [[rx ry] (->dirvec n)]
         (-> ship
             (assoc :dirx (- (* dirx rx) (* diry ry)))
             (assoc :diry (+ (* dirx ry) (* diry rx)))))
    \R (move-ship ship [\L (* -1 n)])
    \F (-> ship
           (assoc :x (+ x (* dirx n)))
           (assoc :y (+ y (* diry n))))))

(defn move-waypoint [{:keys [x y wx wy] :as ship} [action n]]
  (condp = action
    \N (assoc ship :wy (+ wy n))
    \S (assoc ship :wy (- wy n))
    \E (assoc ship :wx (+ wx n))
    \W (assoc ship :wx (- wx n))
    \L (let [dx (- wx x)
             dy (- wy y)
             [rx ry] (->dirvec n)
             dx2 (- (* dx rx) (* dy ry))
             dy2 (+ (* dx ry) (* dy rx)) ]
         (-> ship
             (assoc :wx (+ x dx2))
             (assoc :wy (+ y dy2))))
    \R (move-waypoint ship [\L (* -1 n)])
    \F (let [dx (- wx x)
             dy (- wy y)
             x2 (+ x (* dx n))
             y2 (+ y (* dy n))]
         (-> ship
             (assoc :x x2)
             (assoc :y y2)
             (assoc :wx (+ x2 dx))
             (assoc :wy (+ y2 dy))))))

(def init-ship {:x 0 :y 0 :wx 10 :wy 1 :dirx 1 :diry 0})

(defn distance [{:keys [x y]}]
  (+ (Math/abs x) (Math/abs y)))

(defmulti solve identity)

(defmethod solve 1 [_]
  (distance (reduce move-ship init-ship (parse-input))))

(defmethod solve 2 [_]
  (distance (reduce move-waypoint init-ship (parse-input))))

(solve 2)

;; {:x 0, :y 0, :wx 10, :wy 1, :dirx 1, :diry 0}
;; {:x 100, :y 10, :wx 110, :wy 11, :dirx 1, :diry 0}
;; {:x 100, :y 10, :wx 110, :wy 14, :dirx 1, :diry 0}
;; {:x 170, :y 38, :wx 180, :wy 42, :dirx 1, :diry 0}
;; {:x 170, :y 38, :wx 212, :wy 198, :dirx 1, :diry 0}
;; {:x 632, :y 1798, :wx 674, :wy 1958, :dirx 1, :diry 0})
