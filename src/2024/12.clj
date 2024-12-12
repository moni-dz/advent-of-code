(require '[clojure.string :as str])

(def adjacent-deltas (mapcat #(vector [% 0] [0 %]) [-1 1]))

(defn adjacent-positions [pos]
  (map #(mapv + pos %) adjacent-deltas))

(defn explore-area [grid [pos value] visited]
  (loop [[curr & rest] [pos] seen visited area #{pos}]
    (if-not curr
      [area seen]
      (let [neighbors (->> (adjacent-positions curr)
                           (remove seen)
                           (filterv #(= value (get-in grid % nil))))]
        (recur (into rest neighbors)
               (into seen neighbors)
               (into area neighbors))))))

(defn find-areas [grid]
  (->> (for [i (range (count grid))
             j (range (count (first grid)))]
         [i j])
       (reduce (fn [[areas seen :as acc] pos]
                 (if (seen pos)
                   acc
                   (let [[area new-seen] (explore-area grid [pos (get-in grid pos)] seen)]
                     [(cond-> areas (seq area) (conj {:pos area :val (get-in grid pos)}))
                      new-seen])))
               [[] #{}])
       first))

(defn boundary-count [grid {:keys [pos val]}]
  (->> pos
       (mapcat adjacent-positions)
       (filterv #(not= val (get-in grid % nil)))
       count))

(defn count-edges [pos dir]
  (let [perp [(- (dir 1)) (dir 0)]
        ahead #(mapv + % dir)
        side #(mapv + % perp)
        edge? #(not (pos (ahead %)))
        corner? #(not (pos (side %)))
        tunnel? #(every? pos [(side %) (ahead (side %))])]
    (->> pos
         (filterv #(and (edge? %) (or (corner? %) (tunnel? %))))
         count)))

(let [grid (->> "inputs/2024/12.txt" slurp str/split-lines (mapv vec))
      areas (find-areas grid)
      area-size (comp count :pos)
      score (fn [f] #(* (area-size %) (f %)))]
  [(transduce (map (score #(boundary-count grid %))) + areas)
   (transduce (map (score #(transduce (map (partial count-edges (:pos %)))
                                      + adjacent-deltas)))
              + areas)])
