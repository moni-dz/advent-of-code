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

(defn a* [start end grid-size corrupted return-path?]
  (loop [{:keys [frontier visited predecessors costs]}
         {:frontier (priority-map start (manhattan-dist start end))
          :visited (transient #{})
          :predecessors {}
          :costs {start 0}}]
    (cond
      (empty? frontier)
      nil

      :else
      (let [[current _] (peek frontier)]
        (if (= current end)
          (if return-path?
            (dec (count (reconstruct-path end predecessors)))
            true)
          (let [neighbors (process-neighbors current grid-size corrupted visited)
                current-cost (get costs current)
                new-state (reduce #(update-state %1 %2 current current-cost end)
                                  {:frontier (pop frontier) :costs costs :predecessors predecessors}
                                  neighbors)]
            (recur (-> new-state (assoc :visited (conj! visited current))))))))))

(defn path-exists? [corrupted grid-size num-bytes]
  (let [current-corrupted (set (take num-bytes corrupted))
        start [0 0]
        end [(dec grid-size) (dec grid-size)]]
    (boolean (a* start end grid-size current-corrupted false))))

(defn blocking-byte? [corrupted grid-size byte-idx]
  (and (path-exists? corrupted grid-size (dec byte-idx))
       (not (path-exists? corrupted grid-size byte-idx))))

(defn find-blocking-byte [corrupted grid-size]
  (loop [low 12
         high (count corrupted)]
    (let [mid (quot (+ low high) 2)]
      (cond
        (= low high)
        (nth corrupted (dec low))
        (blocking-byte? corrupted grid-size mid)
        (nth corrupted (dec mid))
        (path-exists? corrupted grid-size mid)
        (recur (inc mid) high)
        :else
        (recur low mid)))))

(let [positions-to-corrupt (parse-input (slurp "inputs/2024/18.txt"))
      bytes-to-corrupt 1024
      dimensions 71
      start [0 0]
      end [(dec dimensions) (dec dimensions)]
      corrupted (set (take bytes-to-corrupt positions-to-corrupt))
      path-length (time (a* start end dimensions corrupted true))
      blocking-byte (time (find-blocking-byte positions-to-corrupt dimensions))]
  [path-length blocking-byte])
