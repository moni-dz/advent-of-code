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

(defn update-frontier [frontier costs node current-cost end]
  (let [tentative-cost (inc current-cost)]
    (if (or (not (contains? costs node)) (< tentative-cost (get costs node)))
      (assoc frontier node (+ tentative-cost (manhattan-dist node end)))
      frontier)))

(defn update-predecessors [predecessors costs node current current-cost]
  (let [tentative-cost (inc current-cost)]
    (if (or (not (contains? costs node)) (< tentative-cost (get costs node)))
      (assoc predecessors node current)
      predecessors)))

(defn update-costs [costs node current-cost]
  (let [tentative-cost (inc current-cost)]
    (if (or (not (contains? costs node)) (< tentative-cost (get costs node)))
      (assoc costs node tentative-cost)
      costs)))

(defn update-estimate [estimate costs node current-cost end]
  (let [tentative-cost (inc current-cost)]
    (if (or (not (contains? costs node)) (< tentative-cost (get costs node)))
      (assoc estimate node (+ tentative-cost (manhattan-dist node end)))
      estimate)))

(defn reconstruct-path [end predecessors]
  (loop [current end
         path []]
    (if (nil? current)
      path
      (recur (get predecessors current)
             (conj path current)))))

(defn process-end-node [end current predecessors return-path?]
  (when (= current end)
    (if return-path?
      (dec (count (reconstruct-path end predecessors)))
      true)))

(defn process-neighbors [current grid-size corrupted closed-set]
  (->> (neighbors current grid-size)
       (remove #(or (corrupted %) (closed-set %)))))

(defn a* [start end grid-size corrupted return-path?]
  (loop [frontier (priority-map start (manhattan-dist start end))
         closed-set #{}
         predecessors {}
         costs {start 0}
         estimate {start (manhattan-dist start end)}]
    (cond
      (empty? frontier)
      nil
      :else
      (let [[current _] (peek frontier)]
        (if-let [result (process-end-node end current predecessors return-path?)]
          result
          (let [neighbors (process-neighbors current grid-size corrupted closed-set)
                current-cost (get costs current)]
            (recur
             (reduce #(update-frontier %1 costs %2 current-cost end) (pop frontier) neighbors)
             (conj closed-set current)
             (reduce #(update-predecessors %1 costs %2 current current-cost) predecessors neighbors)
             (reduce #(update-costs %1 %2 current-cost) costs neighbors)
             (reduce #(update-estimate %1 costs %2 current-cost end) estimate neighbors))))))))

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
