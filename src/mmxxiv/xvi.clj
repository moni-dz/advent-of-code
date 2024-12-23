(ns mmxxiv.xvi
  (:require [clojure.string :as str]
            [clojure.data.priority-map :refer [priority-map]]
            [clojure.set :as set]))

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

(def valid-move?
  (memoize (fn [pos grid]
             (and (in-bounds? pos grid)
                  (not= \# (get-in grid (reverse pos)))))))

(defn turn [[dx dy]]
  [[(- dy) dx] [dy (- dx)]])

(defn dead-end? [pos dir grid]
  (let [forward (mapv + pos dir)
        [left right] (turn dir)]
    (and (not (valid-move? forward grid))
         (not (valid-move? (mapv + pos left) grid))
         (not (valid-move? (mapv + pos right) grid)))))

(def manhattan-dist
  (memoize (fn [pos end] (reduce + (map (comp abs -) pos end)))))

(defn min-remaining-steps [[x y] [u v]]
  (+ (abs (- x u)) (abs (- y v))))

(defn state->key [{:keys [pos dir steps]}] [pos dir steps])

(def vertex-costs (atom {}))

(defn update-vertex-cost! [pos dir cost]
  (swap! vertex-costs
         (fn [costs]
           (let [key [pos dir]]
             (if (and (contains? costs key) (>= cost (get costs key)))
               costs
               (assoc costs key cost))))))

(defn get-vertex-cost [pos dir]
  (get @vertex-costs [pos dir] Double/POSITIVE_INFINITY))

(defn create-forward-successor [{:keys [pos dir cost steps visited]} grid]
  (let [pos (mapv + pos dir)
        cost (inc cost)]
    (when (and (valid-move? pos grid)
               (<= cost (get-vertex-cost pos dir))
               (not (dead-end? pos dir grid)))
      (update-vertex-cost! pos dir cost)
      {:pos pos :dir dir :cost cost :steps (inc steps) :visited (conj visited pos)})))

(defn create-turn-successors [{:keys [pos dir cost visited]}]
  (for [dir (turn dir)
        :let [cost (+ cost 1000)]
        :when (<= cost (get-vertex-cost pos dir))]
    (do
      (update-vertex-cost! pos dir cost)
      {:pos pos :dir dir :cost cost :steps 0 :visited visited})))

(defn valid-successor? [costs successor]
  (let [key (state->key successor)]
    (or (not (costs key)) (<= (:cost successor) (costs key)))))

(defn priority [current-cost heuristic-cost]
  (+ current-cost (* 1.1 heuristic-cost)))

(defn heuristic [end successor]
  (let [h-cost (manhattan-dist (:pos successor) end)]
    [successor (priority (:cost successor) h-cost)]))

(defn create-successors [{:keys [cost pos] :as state} grid end costs score]
  (when (and (< cost score)
             (< (+ cost (manhattan-dist pos end)) (* 1.5 score))
             (< (+ cost
                   (* 1000 (quot (min-remaining-steps pos end) 3))
                   (min-remaining-steps pos end))
                (* 2.0 score)))
    (persistent!
     (reduce
      (fn [acc successor]
        (let [[s priority] (heuristic end successor)]
          (if (valid-successor? costs successor)
            (assoc! acc s priority)
            acc)))
      (transient {})
      (concat
       (when-let [forward (create-forward-successor state grid)]
         [forward])
       (create-turn-successors state))))))

(defn update-scores [score successors]
  (persistent! (reduce-kv
                (fn [s successor _] (assoc! s (state->key successor) (:cost successor)))
                (transient score)
                successors)))

(defn a* [grid start end]
  (loop [frontier (priority-map start (priority 0 (manhattan-dist (:pos start) end)))
         score {(state->key start) 0}
         optimal Double/POSITIVE_INFINITY
         paths []]
    (if (empty? frontier)
      [optimal paths]
      (let [[current _] (peek frontier)
            current-cost (get score (state->key current))]
        (if (= (:pos current) end)
          (if (<= current-cost optimal)
            (recur (pop frontier)
                   score
                   current-cost
                   (if (= current-cost optimal)
                     (conj paths current)
                     [current]))
            (recur (pop frontier) score optimal paths))
          (let [[new-frontier new-scores]
                (if-let [successors (create-successors current grid end score optimal)]
                  [(into (pop frontier) successors) (update-scores score successors)]
                  [(pop frontier) score])]
            (recur new-frontier new-scores optimal paths)))))))

(let [data (str/split-lines (slurp "inputs/2024/16.txt"))
      {:keys [grid start end]} (parse-input data)
      origin (assoc start :cost 0 :steps 0 :visited #{(:pos start)})
      _ (reset! vertex-costs {})
      _ (update-vertex-cost! (:pos start) (:dir start) 0)
      [score paths] (time (a* grid origin end))
      tiles (time (count (apply set/union (map :visited paths))))]
  [score tiles])
