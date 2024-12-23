(ns mmxxiv.viii
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]))

(defn within-grid? [[x y] width height]
  (and (<= 0 x (dec width))
       (<= 0 y (dec height))))

(defn parse-input [input]
  (let [lines (str/split-lines input)
        height (count lines)
        width (count (first lines))
        antennae (for [[y line] (map-indexed vector lines)
                       [x char] (map-indexed vector line)
                       :when (not= char \.)]
                   [[x y] char])]
    {:antennae antennae
     :width width
     :height height}))

(defn line-points [[x1 y1] [x2 y2] width height]
  (let [dx (- x2 x1)
        dy (- y2 y1)
        gcd-val (if (and (zero? dx) (zero? dy))
                  1
                  (.gcd (biginteger dx) (biginteger dy)))
        step [(quot dx gcd-val) (quot dy gcd-val)]
        xf (comp
            (take-while #(within-grid? % width height))
            (map vec))]
    (if (= [x1 y1] [x2 y2])
      #{[x1 y1]}
      (set/union
       (into #{} xf (iterate #(mapv + % step) [x2 y2]))
       (into #{} xf (iterate #(mapv - % step) [x1 y1]))))))

(defn calculate-antinodes [freq-groups width height type]
  (->> freq-groups
       (map (fn [[_ group]]
              (case type
                :mirrored
                (let [positions (map first group)]
                  (->> (for [p1 positions
                             p2 positions
                             :when (not= p1 p2)
                             :let [[x1 y1] p1
                                   [x2 y2] p2
                                   mirror [(- (* 2 x2) x1) (- (* 2 y2) y1)]]
                             :when (within-grid? mirror width height)]
                         mirror)))

                :line
                (->> (combo/combinations group 2)
                     (mapcat (fn [[[pos1 _] [pos2 _]]]
                               (line-points pos1 pos2 width height)))
                     (filter #(within-grid? % width height))))))
       (apply concat)
       set
       count))

(let [{:keys [antennae width height]} (parse-input (slurp "inputs/2024/8.txt"))
      freq-groups (group-by second antennae)
      multi-freq-groups (filter #(>= (count (second %)) 2) freq-groups)]

  [(time (calculate-antinodes multi-freq-groups width height :mirrored))
   (time (calculate-antinodes multi-freq-groups width height :line))])
