(require '[clojure.core.reducers :as r]
         '[clojure.string :as str])

(import java.util.BitSet
        java.util.stream.IntStream
        java.util.function.IntConsumer)

(def directions {\^ [0 -1], \v [0 1], \< [-1 0], \> [1 0]})
(def turn-right {[0 -1] [1 0], [1 0] [0 1], [0 1] [-1 0], [-1 0] [0 -1]})

(defn pos->index [{:keys [width]} [x y]]
  (+ x (* y width)))

(defn index->pos [{:keys [width]} index]
  [(mod index width) (quot index width)])

(defn pos-dir->index [{:keys [width _]} [x y] dir]
  (let [dir-index ({[0 -1] 0
                    [1  0] 1
                    [0  1] 2
                    [-1 0] 3}
                   dir)]
    (+ (* (+ x (* y width)) 4) dir-index)))

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

(defn move [{:keys [pos dir] :as state}]
  (let [next-pos (get-next-position pos dir)]
    (cond
      (not (in-bounds? state next-pos)) (assoc state :pos next-pos :dir dir :leaving true)
      (blocked? state next-pos) (assoc state :dir (turn-right dir))
      :else (assoc state :pos next-pos))))

(def dir-symbols {[0 -1] "\u001B[34m██\u001B[0m"
                  [0 1]  "\u001B[32m██\u001B[0m"
                  [-1 0] "\u001B[33m██\u001B[0m"
                  [1 0]  "\u001B[31m██\u001B[0m"})

(def dir-names {[0 -1] "\u001B[34mUP\u001B[0m"
                [0 1]  "\u001B[32mDOWN\u001B[0m"
                [-1 0] "\u001B[33mLEFT\u001B[0m"
                [1 0]  "\u001B[31mRIGHT\u001B[0m"})

(def BSU "\u001B[?2026h") ; Begin Synchronized Update
(def ESU "\u001B[?2026l") ; End Synchronized Update

(def DISPLAY-HEIGHT 46)
(def DISPLAY-WIDTH 80)
(def LEFT-MARGIN 2)
(def TOP-MARGIN 2)

(defn get-terminal-size []
  [DISPLAY-HEIGHT (* DISPLAY-WIDTH 2)])

(defn draw-grid [state visited cur-pos & [final-message]]
  (let [[rows cols] (get-terminal-size)
        grid-height (count (:grid state))
        grid-width (count (first (:grid state)))
        display-width (quot cols 2)
        display-height (min (- rows 6) display-width)
        [px py] (:pos state)
        guard-symbol (dir-symbols (:dir state))
        obstruction (when (not= cur-pos (:pos state)) cur-pos)

        view-half-width (quot display-width 2)
        view-half-height (quot display-height 2)

        start-x (max 0 (min (- grid-width display-width) (- px view-half-width)))
        end-x (min grid-width (+ start-x display-width))
        start-x (max 0 (- end-x display-width))

        start-y (max 0 (min (- grid-height display-height) (- py view-half-height)))
        end-y (min grid-height (+ start-y display-height))
        start-y (max 0 (- end-y display-height))

        padding-str (apply str (repeat LEFT-MARGIN " "))
        sb (StringBuilder.)]

    (.append sb BSU)
    (.append sb "\u001B[2J")
    (.append sb "\u001B[3J")
    (.append sb "\u001B[H")

    (.append sb (format "Position: %-20s\n" (str (:pos state))))
    (.append sb (format "Direction: %-20s\n" (dir-names (:dir state))))
    (.append sb (format "Visited positions: %-20d\n" (count visited)))
    (when final-message
      (.append sb (str final-message "\n")))

    (dotimes [_ (if final-message (dec TOP-MARGIN) TOP-MARGIN)]
      (.append sb "\n"))

    (doseq [y (range start-y end-y)]
      (.append sb padding-str)
      (doseq [x (range start-x end-x)]
        (let [abs-pos [x y]]
          (.append sb
                   (cond
                     (= [x y] [px py]) guard-symbol
                     (and obstruction (= abs-pos obstruction)) "\u001B[38;5;129m██\u001B[0m"
                     (= \# (get-in (:grid state) [y x])) "\u001B[35m██\u001B[0m"
                     (visited abs-pos) "▓▓"
                     :else "░░"))))
      (.append sb "\n"))

    (.append sb ESU)
    (print (.toString sb))
    (flush)))

(defn simulate [input]
  (let [initial-state (parse-input input)
        {:keys [height width]} initial-state
        grid-size (* height width)
        visited-bitset (java.util.BitSet. grid-size)
        seen-states (java.util.BitSet. (* grid-size 4))
        initial-pos (:pos initial-state)
        initial-key (pos-dir->index initial-state initial-pos (:dir initial-state))]

    (print "\u001B[?25l")
    (print "\u001B[2J")
    (print "\u001B[3J")
    (print "\u001B[H")
    (flush)

    (.set visited-bitset (pos->index initial-state initial-pos))
    (.set seen-states initial-key)

    (loop [state initial-state
           visit-count 1]
      (let [visited (into #{}
                          (for [i (range (.size visited-bitset))
                                :when (.get visited-bitset i)]
                            (index->pos initial-state i)))
            new-state (move state)
            new-pos (:pos new-state)
            new-dir (:dir new-state)
            new-key (pos-dir->index initial-state new-pos new-dir)
            pos-idx (pos->index initial-state new-pos)]

        (cond
          (:leaving new-state)
          (do
            (draw-grid state visited (:pos state) (str "Guard left at: " new-pos))
            (Thread/sleep 1000)  ; Give time to see final state
            (print "\u001B[?25h")
            visit-count)

          (.get seen-states new-key)
          (do
            (draw-grid state visited (:pos state) "Loop detected!")
            (Thread/sleep 1000)  ; Give time to see final state
            (print "\u001B[?25h")
            visit-count)

          :else
          (do
            (draw-grid state visited (:pos state))
            (Thread/sleep 1)
            (.set visited-bitset pos-idx)
            (.set seen-states new-key)
            (recur new-state (inc visit-count))))))))

(defn add-obstruction [grid [x y]]
  (update grid y #(str (subs % 0 x) "#" (subs % (inc x)))))

(defn simulate-with-obstruction [input pos]
  (let [initial-state (-> (parse-input input)
                          (update :grid #(add-obstruction % pos)))
        {:keys [height width]} initial-state
        ^java.util.BitSet seen-states (BitSet. (* height width 4))
        initial-key (pos-dir->index initial-state (:pos initial-state) (:dir initial-state))]

    (.set seen-states initial-key)

    (loop [state initial-state
           prev-dir (:dir initial-state)
           steps 0
           collision-count 0]
      (let [new-state (move state)
            new-dir (:dir new-state)
            hit-obstacle? (not= prev-dir new-dir)
            state-index (pos-dir->index initial-state (:pos new-state) new-dir)]
        (cond
          (:leaving new-state) false
          (>= steps 10000) false

          (and hit-obstacle?
               (.get seen-states state-index)
               (pos? collision-count))
          true

          :else
          (do
            (.set seen-states state-index)
            (recur new-state
                   new-dir
                   (inc steps)
                   (if hit-obstacle?
                     (inc collision-count)
                     collision-count))))))))

(defn combine-results
  ([] #{})
  ([s1 s2] (into s1 s2)))

(defn get-visited-positions [input]
  (let [initial-state (parse-input input)
        {:keys [grid height width]} initial-state
        grid-size (* height width)
        row-obstacles (BitSet. height)
        col-obstacles (BitSet. width)]

    (.. (IntStream/range 0 height)
        (parallel)
        (forEach (reify IntConsumer
                   (accept [_ y]
                     (dotimes [x width]
                       (when (= \# (get-in grid [y x]))
                         (.set row-obstacles y)
                         (.set col-obstacles x)))))))

    (loop [state initial-state
           visited (BitSet. grid-size)
           seen-states #{[(:pos initial-state) (:dir initial-state)]}]
      (let [new-state (move state)
            new-key [(:pos new-state) (:dir new-state)]
            curr-index (pos->index state (:pos new-state))]
        (cond
          (or (:leaving new-state) (seen-states new-key))
          (let [result (BitSet. grid-size)
                grid-ref (volatile! grid)
                initial-pos-ref (volatile! (:pos initial-state))]
            (.. (IntStream/range 0 grid-size)
                (parallel)
                (forEach
                 (reify IntConsumer
                   (accept [_ i]
                     (let [[x y] (index->pos state i)]
                       (when (and (.get visited i)
                                  (= \. (get-in @grid-ref [y x]))
                                  (not= [x y] @initial-pos-ref)
                                  (or (.get row-obstacles y)
                                      (.get col-obstacles x)))
                         (.set result i)))))))
            result)

          :else
          (do
            (.set visited curr-index)
            (recur new-state
                   visited
                   (conj seen-states new-key))))))))

(defn bitset->positions [state bitset]
  (for [i (range (.size bitset))
        :when (.get bitset i)]
    (index->pos state i)))

(defn process-batch [input positions batch-size]
  (let [size (count positions)
        results (BitSet. size)
        pos-array (object-array size)]

    (dotimes [i size]
      (aset pos-array i (nth positions i)))

    (dotimes [i (quot (+ size (dec batch-size)) batch-size)]
      (let [start (* i batch-size)
            end (min size (+ start batch-size))]
        (dotimes [j (- end start)]
          (let [idx (+ start j)
                pos (aget pos-array idx)]
            (when (simulate-with-obstruction input pos)
              (.set results idx))))))

    (into #{}
          (keep-indexed (fn [idx _]
                          (when (.get results idx)
                            (aget pos-array idx)))
                        (range size)))))

(defn find-loop-positions [input]
  (let [initial-state (parse-input input)
        visited-bitset (get-visited-positions input)
        positions (vec (bitset->positions initial-state visited-bitset))
        total-positions (count positions)
        counter (atom 0)
        results (atom #{})
        n-cores (.. Runtime getRuntime availableProcessors)
        positions-per-core (/ total-positions n-cores)
        min-chunk-size 50
        max-chunk-size 450
        target-chunks-per-core 12
        chunk-size (-> positions-per-core
                       (/ target-chunks-per-core)
                       (max min-chunk-size)
                       (min max-chunk-size)
                       int)
        batch-size 16
        fold-threshold (max 1 (quot chunk-size 2))
        chunks (partition-all chunk-size positions)
        total-chunks (count chunks)]

    (print "\u001B[?25l\n\n")
    (flush)

    (let [final-results
          (->> chunks
               (r/fold
                fold-threshold
                combine-results
                (fn [acc chunk]
                  (let [curr (swap! counter inc)]
                    (when (zero? (mod curr 1))
                      (print (format "\u001B[2A\rProgress: %.1f%% (%d/%d)\nFound: %d valid positions\n"
                                     (* 100.0 (/ curr total-chunks))
                                     (min total-positions (* curr chunk-size))
                                     total-positions
                                     (count @results)))
                      (flush))

                    (let [chunk-results (process-batch input chunk batch-size)]
                      (swap! results into chunk-results)
                      (into acc chunk-results))))))]

      (print (format "\u001B[2A\rProgress: 100.0%% (%d/%d)\nFound: %d valid positions\n"
                     total-positions total-positions
                     (count final-results)))
      (print "\u001B[?25h")
      final-results)))

(let [input (slurp "6.txt")]
  (print "Choose part (1/2): ")
  (flush)
  (case (read-line)
    "1" (simulate input)
    "2" (time (find-loop-positions input))
    (println "Invalid choice.")))
