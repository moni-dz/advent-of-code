(require '[clojure.core.reducers :as r]
         '[clojure.string :as str])

(def directions {\^ [0 -1], \v [0 1], \< [-1 0], \> [1 0]})

(defn parse-input [input]
  (let [grid (vec (str/split-lines input))
        [y x dir] (first (for [y (range (count grid))
                               x (range (count (first grid)))
                               :when (directions (get-in grid [y x]))]
                           [y x (get-in grid [y x])]))]
    {:grid grid
     :height (count grid)
     :width (count (first grid))
     :pos [x y]
     :dir (directions dir)}))

(defn in-bounds? [{:keys [height width]} [x y]]
  (and (>= x 0) (< x width) (>= y 0) (< y height)))

(defn blocked? [{:keys [grid]} [x y]]
  (= \# (get-in grid [y x])))

(defn get-next-position [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(def turn-right {[0 -1] [1 0], [1 0] [0 1], [0 1] [-1 0], [-1 0] [0 -1]})

(defn move [{:keys [pos dir] :as state}]
  (let [next-pos (get-next-position pos dir)]
    (cond
      (not (in-bounds? state next-pos)) (assoc state :pos next-pos :dir dir :leaving true)
      (blocked? state next-pos) (assoc state :dir (turn-right dir))
      :else (assoc state :pos next-pos))))

(defn simulate [input]
  (let [initial-state (parse-input input)]
    (print "\u001B[?25l")
    (println "\n\n")
    (flush)

    (loop [state initial-state
           visited #{(:pos initial-state)}
           seen-states #{[(:pos initial-state) (:dir initial-state)]}]

      (print (format "\u001B[3A\rPosition: %-20s\nDirection: %-20s\nVisited positions: %-20d\n"
                     (str (:pos state))
                     (str (:dir state))
                     (count visited)))
      (flush)

      (let [new-state (move state)
            new-key [(:pos new-state) (:dir new-state)]]
        (cond
          (:leaving new-state)
          (do
            (print "\u001B[?25h")
            (println "Guard left at:" (:pos new-state))
            (count visited))

          (seen-states new-key)
          (do
            (print "\u001B[?25h")
            (println "Loop detected!")
            (count visited))

          :else (recur new-state
                       (conj visited (:pos new-state))
                       (conj seen-states new-key)))))))

(defn add-obstruction [grid [x y]]
  (update grid y #(str (subs % 0 x) "#" (subs % (inc x)))))

(defn cycle? [state seen-states steps]
  (let [key [(:pos state) (:dir state)]
        first-seen (seen-states key)]
    (when first-seen
      (let [loop-length (- steps first-seen)]
        (and (pos? first-seen) (> loop-length 1))))))

(defn simulate-with-obstruction [input pos]
  (let [initial-state (-> (parse-input input)
                          (update :grid #(add-obstruction % pos)))]
    (loop [state initial-state
           seen-states {[(:pos initial-state) (:dir initial-state)] 0}
           steps 0]
      (let [new-state (move state)]
        (cond
          (:leaving new-state) false
          (>= steps 10000) false
          :else (let [key [(:pos new-state) (:dir new-state)]]
                  (if (cycle? new-state seen-states steps)
                    true
                    (recur new-state
                           (assoc seen-states key steps)
                           (inc steps)))))))))

(defn process-position [input counter results positions]
  (fn [acc pos]
    (let [curr (swap! counter inc)]
      (when (zero? (mod curr 100))
        (print (format "\u001B[2A\rProgress: %.1f%% (%d/%d)\nFound: %d valid positions\n"
                       (* 100.0 (/ curr (count positions)))
                       curr (count positions)
                       (inc (count @results))))
        (flush))
      (if (simulate-with-obstruction input pos)
        (do
          (swap! results conj pos)
          (conj acc pos))
        acc))))

(defn reduce-chunk [input counter results positions]
  (fn [acc chunk]
    (reduce (process-position input counter results positions) acc chunk)))

(defn combine-results
  ([] #{})
  ([s1 s2] (into s1 s2)))

(defn find-loop-positions [input]
  (let [initial-state (parse-input input)
        positions (vec (for [y (range (:height initial-state))
                             x (range (:width initial-state))
                             :when (and (= \. (get-in (:grid initial-state) [y x]))
                                        (not= [x y] (:pos initial-state)))]
                         [x y]))
        n-cores (.. Runtime getRuntime availableProcessors)
        chunk-size (max 1 (quot (count positions) n-cores))
        counter (atom 0)
        results (atom #{})
        chunks (->> positions
                    (partition-all chunk-size)
                    (vec))]

    (print "\u001B[?25l\n\n")
    (flush)

    (->> chunks
         (r/fold 1
                 combine-results
                 (reduce-chunk input counter results positions)))

    (print (format "\u001B[2A\rProgress: 100.0%% (%d/%d)\nFound: %d valid positions\n"
                   (count positions) (count positions)
                   (count @results)))
    (print "\u001B[?25h")))

(let [input (slurp "6.txt")]
  (print "Choose part (1/2): ")
  (flush)
  (case (read-line)
    "1" (simulate input)
    "2" (find-loop-positions input)
    (println "Invalid choice.")))
