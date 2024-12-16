(require '[clojure.string :as str]
         '[clojure.data.priority-map :refer [priority-map]]
         '[clojure.set :as set])

(defn find-marker [grid marker]
  (first (for [y (range (count grid))
               x (range (count (first grid)))
               :when (= marker (get-in grid [y x]))]
           [x y])))

(defn parse-input [lines]
  (let [grid (mapv vec lines)]
    {:grid grid
     :start {:pos (find-marker grid \S) :dir [1 0]}
     :goal (find-marker grid \E)}))

(defn in-bounds? [[x y] grid]
  (let [max-x (dec (count (first grid)))
        max-y (dec (count grid))]
    (and (<= 0 x max-x) (<= 0 y max-y))))

(defn valid-move? [pos grid]
  (and (in-bounds? pos grid)
       (not= \# (get-in grid (reverse pos)))))

(defn turn [[dx dy]]
  [[(- dy) dx] [dy (- dx)]])

(def manhattan-dist
  (memoize (fn [pos goal] (reduce + (map (comp abs -) pos goal)))))

(defn state->key [{:keys [pos dir steps]}] [pos dir steps])

(defn create-successors [{:keys [pos dir cost steps visited]} grid goal costs]
  (let [forward (mapv + pos dir)
        [left right] (turn dir)]
    (->> (concat
          (when (valid-move? forward grid)
            [{:pos forward :dir dir :cost (inc cost) :steps (inc steps) :visited (conj visited forward)}])
          (for [new-dir [left right]]
            {:pos pos :dir new-dir :cost (+ cost 1000) :steps 0 :visited visited}))
         (filter #(let [key (state->key %)]
                    (or (not (costs key))
                        (<= (:cost %) (costs key)))))
         (map #(vector % (+ (:cost %) (manhattan-dist (:pos %) goal))))
         (into {}))))

(defn a* [grid start goal]
  (loop [frontier (priority-map start (manhattan-dist (:pos start) goal))
         score {(state->key start) 0}
         best-score Double/POSITIVE_INFINITY
         best-paths []]
    (if (empty? frontier)
      [best-score best-paths]
      (let [[current _] (peek frontier)
            {current-cost (state->key current)} score]
        (if (= (:pos current) goal)
          (if (<= current-cost best-score)
            (recur (pop frontier)
                   score
                   current-cost
                   (if (= current-cost best-score)
                     (conj best-paths current)
                     [current]))
            (recur (pop frontier) score best-score best-paths))
          (let [[new-frontier new-scores]
                (-> current
                    (create-successors grid goal score)
                    (as-> successors
                          [(into (pop frontier) successors)
                           (into score (for [[next _] successors] [(state->key next) (:cost next)]))]))]
            (recur new-frontier new-scores best-score best-paths)))))))

(let [data (str/split-lines (slurp "inputs/2024/16.txt"))
      {:keys [grid start goal]} (parse-input data)
      initial-state (assoc start :cost 0 :steps 0 :visited #{(:pos start)})
      [score paths] (a* grid initial-state goal)
      tiles (count (apply set/union (map :visited paths)))]
  [score tiles])
