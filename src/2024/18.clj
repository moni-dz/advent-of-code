(require '[clojure.string :as str]
         '[clojure.data.priority-map :refer [priority-map]])

(defn parse-input [input]
  (->> input str/split-lines (map (comp #(mapv parse-long %) #(str/split % #",")))))

(def manhattan-dist
  (memoize (fn [pos end] (reduce + (map (comp abs -) pos end)))))

(defn in-bounds? [[x y] grid-size]
  (and (<= 0 x (dec grid-size)) (<= 0 y (dec grid-size))))

(defn neighbors [[x y] grid-size]
  (for [[dx dy] [[0 1] [1 0] [0 -1] [-1 0]]
        :let [nx (+ x dx) ny (+ y dy)]
        :when (in-bounds? [nx ny] grid-size)]
    [nx ny]))

(defn should-update? [costs node current-cost]
  (let [cost (inc current-cost)]
    (or (not (contains? costs node)) (< cost (get costs node)))))

(defn update-state [state node current current-cost end]
  (let [cost (inc current-cost)]
    (if (should-update? (:costs state) node current-cost)
      (-> state
          (update :frontier assoc node (+ cost (manhattan-dist node end)))
          (update :costs assoc node cost)
          (update :predecessors assoc node current))
      state)))

(defn reconstruct-path [end predecessors]
  (loop [current end
         path (transient [])]
    (if (nil? current)
      (persistent! path)
      (recur (get predecessors current) (conj! path current)))))

(defn process-neighbors [current grid-size corrupted visited]
  (->> (neighbors current grid-size)
       (remove #(or (corrupted %) (contains? visited %)))))

(defn a* [start end grid-size corrupted]
  (loop [{:keys [frontier visited predecessors costs]}
         {:frontier (priority-map start (manhattan-dist start end))
          :visited (transient #{})
          :predecessors {}
          :costs {start 0}}]
    (if (empty? frontier)
      nil
      (let [[current _] (peek frontier)]
        (if (= current end)
          (dec (count (reconstruct-path end predecessors)))
          (let [neighbors (process-neighbors current grid-size corrupted visited)
                current-cost (get costs current)
                new-state (reduce #(update-state %1 %2 current current-cost end)
                                  {:frontier (pop frontier) :costs costs :predecessors predecessors}
                                  neighbors)]
            (recur (-> new-state (assoc :visited (conj! visited current))))))))))

(defn root [parent x]
  (let [p (get @parent x)]
    (if (= x p)
      x
      (let [root (root parent p)]
        (vreset! parent (assoc! @parent x root))
        root))))

(defn union! [parent x y]
  (let [[x y] [(root parent x) (root parent y)]]
    (when (not= x y) (vreset! parent (assoc! @parent x y)))))

(defn connected? [parent start end]
  (= (root parent start) (root parent end)))

(defn find-blocking-byte [corrupted grid-size]
  (let [parent (volatile!
                (transient
                 (into {} (for [x (range grid-size)
                                y (range grid-size)]
                            [[x y] [x y]]))))
        start [0 0]
        end [(dec grid-size) (dec grid-size)]
        corrupted-set (volatile! (disj (set corrupted) start end))]

    (doseq [x (range grid-size)
            y (range grid-size)
            :let [curr [x y]]
            :when (not (@corrupted-set curr))]
      (doseq [neighbor (neighbors curr grid-size)
              :when (not (@corrupted-set neighbor))]
        (union! parent curr neighbor)))

    (loop [idx (dec (count corrupted))]
      (when (>= idx 0)
        (let [pos (nth corrupted idx)]
          (when-not (or (= pos start) (= pos end))
            (vswap! corrupted-set disj pos)
            (doseq [neighbor (neighbors pos grid-size)
                    :when (not (@corrupted-set neighbor))]
              (union! parent pos neighbor))

            (if (connected? parent start end)
              pos
              (recur (dec idx)))))))))

(let [positions-to-corrupt (parse-input (slurp "inputs/2024/18.txt"))
      bytes-to-corrupt 1024
      dimensions 71
      start [0 0]
      end [(dec dimensions) (dec dimensions)]
      corrupted (set (take bytes-to-corrupt positions-to-corrupt))
      path-length (time (a* start end dimensions corrupted))
      blocking-byte (time (find-blocking-byte positions-to-corrupt dimensions))]
  [path-length blocking-byte])
