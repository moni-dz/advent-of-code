(ns mmxxiv.xv
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (let [[grid moves] (str/split input #"\n\n")]
    [(mapv vec (str/split-lines grid)) moves]))

(defn create-wide-grid [grid]
  (let [wide {\# [\# \#] \O [\[ \]] \. [\. \.] \@ [\@ \.]}]
    (mapv (fn [row] (vec (mapcat wide row))) grid)))

(defn find-robot [grid]
  (first
   (for [x (range (count grid)) y (range (count (first grid)))
         :when (= (get-in grid [x y]) \@)]
     [x y])))

(defn get-affected-pos [grid [x y :as pos] [dx _]]
  (case (get-in grid pos)
    (\@ \O) [pos]
    \[ (if (zero? dx) [pos] [pos [x (inc y)]])
    \] (if (zero? dx) [pos] [pos [x (dec y)]])
    []))

(defn update-grid [grid pos [dx dy]]
  (reduce
   (fn [g [x y :as new-pos]]
     (let [old-pos [(- x dx) (- y dy)]]
       (assoc-in (assoc-in g new-pos (get-in grid old-pos)) old-pos \.)))
   grid pos))

(defn push [grid pos [dx dy :as delta]]
  (when-not (some #(= (get-in grid %) \#) pos)
    (let [pushed (set (for [p pos [x y] (get-affected-pos grid p delta)] [(+ x dx) (+ y dy)]))]
      (if (every? #(= (get-in grid %) \.) pushed)
        (update-grid grid pushed delta)
        (some-> (push grid pushed delta)
                (update-grid pushed delta))))))

(defn move [[grid [x y :as pos]] c]
  (let [direction {\v [1 0] \^ [-1 0] \> [0 1] \< [0 -1]}]
    (if (= c \newline)
      [grid pos]
      (let [[dx dy :as delta] (direction c [0 0])]
        (if-let [modified (push grid #{pos} delta)]
          [modified [(+ x dx) (+ y dy)]]
          [grid pos])))))

(defn solve [grid box]
  (reduce + (for [x (range (count grid)) y (range (count (first grid)))
                  :when (= (get-in grid [x y]) box)]
              (+ (* 100 x) y))))

(let [[grid moves] (parse-input (slurp "inputs/2024/15.txt"))
      wide (create-wide-grid grid)
      [x1 y1 :as pos] (find-robot grid)
      wide-pos [x1 (* y1 2)]
      [grid _] (reduce move [grid pos] moves)
      [wide _] (reduce move [wide wide-pos] moves)]
  [(solve grid \O) (solve wide \[)])
