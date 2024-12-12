(require '[clojure.string :as str])

(def deltas [[1 0] [-1 0] [0 1] [0 -1]])
(def adjacent #(map (partial mapv + %) deltas))

(defn explore [grid [pos value] visited]
  (loop [[curr & rest] [pos], seen visited, region #{pos}]
    (if-not curr
      [region seen]
      (let [adjs (->> (adjacent curr) (remove seen) (filter #(= value (get-in grid % nil))))]
        (recur (into rest adjs) (into seen adjs) (into region adjs))))))

(defn areas [grid]
  (->> (for [i (range (count grid)), j (range (count (first grid)))] [i j])
       (reduce (fn [[regions visited] pos]
                 (if (visited pos)
                   [regions visited]
                   (let [[region seen] (explore grid [pos (get-in grid pos)] visited)]
                     [(conj regions {:pos region :value (get-in grid pos)}) seen])))
               [[] #{}])
       first))

(defn boundaries [grid {:keys [pos value]}]
  (->> pos (mapcat adjacent) (filter #(not= value (get-in grid % nil))) count))

(defn edges [pos dir]
  (let [perp [(- (dir 1)) (dir 0)]
        ahead #(mapv + % dir), side #(mapv + % perp)
        edge? #(not (pos (ahead %))), corner? #(not (pos (side %)))
        tunnel? #(every? pos [(side %) (ahead (side %))])]
    (->> pos (filterv #(and (edge? %) (or (corner? %) (tunnel? %)))) count)))

(let [grid (->> "inputs/2024/12.txt" slurp str/split-lines (mapv vec))
      price (fn [f a] (* (count (:pos a)) (f a)))]
  [(transduce (map #(price (partial boundaries grid) %)) + (areas grid))
   (transduce (map #(price (fn [a] (reduce + (map (partial edges (:pos a)) deltas))) %)) + (areas grid))])
