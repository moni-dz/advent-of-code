(require '[clojure.string :as str]
         '[clojure.set :as set])

(defn gcd
  [a b]
  (if (zero? b)
    (abs a)
    (recur b (mod a b))))

(defn within-grid?
  [[x y] width height]
  (and (integer? x)
       (integer? y)
       (<= 0 x (dec width))
       (<= 0 y (dec height))))

(defn unique-pairs
  [coll]
  (for [i (range (count coll))
        j (range (inc i) (count coll))]
    [(nth coll i) (nth coll j)]))

(defn parse-input
  [input]
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

(defn group-antennae
  [antennae]
  (group-by second antennae))

(defn calculate-mirrored-antinodes
  [[[x1 y1] _] [[x2 y2] _] width height]
  (let [c1 [(- (* 2 x2) x1)
            (- (* 2 y2) y1)]
        c2 [(- (* 2 x1) x2)
            (- (* 2 y1) y2)]]
    (filter #(within-grid? % width height) [c1 c2])))

(defn line-points
  [[x1 y1] [x2 y2] width height]
  (let [dx (- x2 x1)
        dy (- y2 y1)
        gcd-val (if (and (zero? dx) (zero? dy)) 1 (gcd dx dy))
        step-x (if (zero? dx) 0 (quot dx gcd-val))
        step-y (if (zero? dy) 0 (quot dy gcd-val))]
    (if (and (zero? dx) (zero? dy))
      [[x1 y1]]
      (let [forward (->> [x2 y2]
                         (iterate #(mapv + % [step-x step-y]))
                         (take-while #(within-grid? % width height)))
            backward (->> [x1 y1]
                          (iterate #(mapv - % [step-x step-y]))
                          (take-while #(within-grid? % width height)))]
        (set/union (set forward) (set backward))))))

(defn calculate-line-antinodes
  [group width height]
  (->> (unique-pairs group)
       (mapcat (fn [[[pos1 _] [pos2 _]]]
                 (line-points pos1 pos2 width height)))
       (filter #(within-grid? % width height))
       set))

(let [{:keys [antennae width height]} (parse-input (slurp "inputs/2024/8.txt"))
      freq-groups (group-antennae antennae)
      multi-freq-groups (filter #(>= (count (second %)) 2) freq-groups)]

  [(->> multi-freq-groups
        (mapcat (fn [[_ group]]
                  (for [[ant1 ant2] (unique-pairs group)]
                    (calculate-mirrored-antinodes ant1 ant2 width height))))
        (apply concat)
        set
        count)

   (->> multi-freq-groups
        (mapcat (fn [[_ group]]
                  (calculate-line-antinodes group width height)))
        set
        count)])
