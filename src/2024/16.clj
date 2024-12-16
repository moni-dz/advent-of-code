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
     :end (find-marker grid \E)}))

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
  (memoize (fn [pos end] (reduce + (map (comp abs -) pos end)))))

(defn state->key [{:keys [pos dir steps]}] [pos dir steps])

(def vertex-costs (atom {}))

(defn update-vertex-cost! [pos dir cost]
  (swap! vertex-costs
         (fn [costs]
           (let [key [pos dir]]
             (if (and (contains? costs key)
                      (>= cost (costs key)))
               costs
               (assoc costs key cost))))))

(defn get-vertex-cost [pos dir]
  (get @vertex-costs [pos dir] Double/POSITIVE_INFINITY))

(defn create-successors [{:keys [pos dir cost steps visited]} grid end costs]
  (let [forward (mapv + pos dir)
        [left right] (turn dir)
        straight-bonus (* 40 (inc steps))
        turn-penalty (* 5 (inc steps))]
    (->> (concat
          (when (valid-move? forward grid)
            (let [new-cost (inc cost)]
              (when (<= new-cost (get-vertex-cost forward dir))
                [(do
                   (update-vertex-cost! forward dir new-cost)
                   {:pos forward :dir dir :cost new-cost :steps (inc steps) :visited (conj visited forward)})])))
          (for [new-dir [left right]
                :let [new-cost (+ cost 1000)]
                :when (<= new-cost (get-vertex-cost pos new-dir))]
            (do
              (update-vertex-cost! pos new-dir new-cost)
              {:pos pos :dir new-dir :cost new-cost :steps 0 :visited visited})))
         (filter #(let [key (state->key %)]
                    (or (not (costs key))
                        (<= (:cost %) (costs key)))))
         (map #(vector % (+ (:cost %)
                            (manhattan-dist (:pos %) end)
                            (if (= (:dir %) dir)
                              (- straight-bonus)
                              turn-penalty))))
         (into {}))))

(defn a* [grid start end]
  (loop [frontier (priority-map start (manhattan-dist (:pos start) end))
         score {(state->key start) 0}
         best-score Double/POSITIVE_INFINITY
         best-paths []]
    (if (empty? frontier)
      [best-score best-paths]
      (let [[current _] (peek frontier)
            {current-cost (state->key current)} score]
        (if (= (:pos current) end)
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
                    (create-successors grid end score)
                    (as-> successors
                          [(into (pop frontier) successors)
                           (into score (for [[next _] successors] [(state->key next) (:cost next)]))]))]
            (recur new-frontier new-scores best-score best-paths)))))))

(let [data (str/split-lines (slurp "inputs/2024/16.txt"))
      {:keys [grid start end]} (parse-input data)
      origin (assoc start :cost 0 :steps 0 :visited #{(:pos start)})
      _ (reset! vertex-costs {})
      _ (update-vertex-cost! (:pos start) (:dir start) 0)
      [score paths] (time (a* grid origin end))
      tiles (time (count (apply set/union (map :visited paths))))]
  [score tiles])
