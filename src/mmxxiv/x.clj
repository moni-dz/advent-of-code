(ns mmxxiv.x
  (:require [clojure.string :as str]
            [lib.core :as lib]))

(defn is-peak? [height] (= height 9))

(defn get-cell [grid y x]
  (when (lib/in-bounds? [x y] grid true)
    (- (int (lib/char-at grid [x y])) 48)))

(defn find-trailheads [grid]
  (for [y (range (count grid))
        x (range (count (first grid)))
        :when (zero? (get-cell grid y x))]
    [y x]))

(defn neighbors [[y x] grid]
  (filter (fn [[ny nx]]
            (get-cell grid ny nx))
          [[(dec y) x] [(inc y) x]
           [y (dec x)] [y (inc x)]]))

(def count-paths
  (memoize
   (fn [grid [y x :as pos]]
     (let [height (get-cell grid y x)]
       (if (= height 9)
         1
         (->> (neighbors pos grid)
              (filter #(= (get-cell grid (first %) (second %))
                          (inc height)))
              (map #(count-paths grid %))
              (reduce + 0)))))))

(defn valid-position? [visited grid [y x :as pos] current-height]
  (and (not (visited pos))
       (= (get-cell grid y x) (inc current-height))))

(defn get-valid-neighbors [visited grid pos height]
  (filter #(valid-position? visited grid % height)
          (neighbors pos grid)))

(defn process-trailhead [grid start]
  (loop [state {:visited #{start}
                :queue [start]
                :peaks #{}}]

    (if (empty? (:queue state))
      [(count (:peaks state)) (count-paths grid start)]

      (let [current-pos (first (:queue state))
            current-height (get-cell grid (first current-pos) (second current-pos))
            rest-queue (rest (:queue state))]

        (if (is-peak? current-height)
          (recur {:visited (:visited state)
                  :queue rest-queue
                  :peaks (conj (:peaks state) current-pos)})

          (let [next-positions (get-valid-neighbors (:visited state) grid current-pos current-height)]
            (recur {:visited (into (:visited state) next-positions)
                    :queue (into rest-queue next-positions)
                    :peaks (:peaks state)})))))))

(def process-trailhead! (memoize process-trailhead))

(let [grid (->> (slurp "inputs/2024/10.txt")
                str/split-lines
                (mapv vec))
      results (time (map #(process-trailhead! grid %) (find-trailheads grid)))]
  [(reduce + (pmap first results))
   (reduce + (pmap second results))])
