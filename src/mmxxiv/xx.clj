(ns mmxxiv.xx
  (:require [clojure.string :as str]
            [clojure.core.reducers :as r]))

(defn parse-input [input]
  (->> input str/split-lines
       (map-indexed (fn [y row] (keep-indexed (fn [x c] (when (not= c \#) [[x y] c])) row)))
       (into {} cat)))

(defn manhattan [[x y] [u v]]
  (+ (abs (- u x)) (abs (- v y))))

(defn build-distances [grid]
  (let [start (ffirst (filter #(= (val %) \S) grid))]
    (loop [todo [start], dist {start 0}]
      (if-let [pos (first todo)]
        (let [new-points (for [d [[0 1] [1 0] [0 -1] [-1 0]]
                               :let [new (mapv + pos d)]
                               :when (and (grid new) (not (dist new)))]
                           [new (inc (dist pos))])]
          (recur (into (subvec todo 1) (map first new-points)) (into dist new-points)))
        dist))))

(defn check-point [dist max-dist min-save [x y :as p]]
  (count (for [dx (range (- max-dist) (inc max-dist))
               dy (range (- max-dist) (inc max-dist))
               :let [q [(+ x dx) (+ y dy)] d (manhattan p q) [dq dp] [(dist q) (dist p)]]
               :when (and dq (< d (inc max-dist)) (> dq dp) (>= (- dq dp d) min-save))] 1)))

(defn valid-cheats [dist max-dist min-save]
  (->> dist keys vec (r/fold 8 + #(+ %1 (check-point dist max-dist min-save %2)))))

(-> "inputs/2024/20.txt" slurp parse-input build-distances
    (as-> d [(time (valid-cheats d 2 100)) (time (valid-cheats d 20 100))]))
