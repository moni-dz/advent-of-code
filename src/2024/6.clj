(require '[clojure.string :as str])

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

(def DISPLAY-HEIGHT 38)
(def DISPLAY-WIDTH 76)
(def LEFT-MARGIN 2)
(def TOP-MARGIN 3)

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

(defn get-visited-positions [input]
  (let [initial-state (parse-input input)]
    (loop [state initial-state
           visited #{}
           seen #{[(:pos initial-state) (:dir initial-state)]}]
      (let [new-state (move state)
            new-key [(:pos new-state) (:dir new-state)]]
        (if (or (:leaving new-state) (seen new-key))
          (conj visited (:pos initial-state)) ; include starting position
          (recur new-state
                 (conj visited (:pos new-state))
                 (conj seen new-key)))))))

(defn forms-cycle? [state pos]
  (let [state-with-block (update state :grid #(add-obstruction % pos))]
    (loop [current-state state-with-block
           seen #{[(:pos state-with-block) (:dir state-with-block)]}]
      (let [next-state (move current-state)
            next-key [(:pos next-state) (:dir next-state)]]
        (cond
          (:leaving next-state) false
          (seen next-key) true
          :else (recur next-state
                       (conj seen next-key)))))))

(defn find-loop-positions [input]
  (let [initial-state (parse-input input)
        start-pos (:pos initial-state)
        visited (get-visited-positions input)]
    (->> visited
         (filter #(and (not= % start-pos)
                       (= \. (get-in (:grid initial-state) [(second %) (first %)]))))
         (pmap #(when (forms-cycle? initial-state %) %))
         (filter identity)
         count)))

(let [input (slurp "inputs/2024/6.txt")]
  (print "Choose part (1/2): ")
  (flush)
  (case (read-line)
    "1" (simulate input)
    "2" (time (println (find-loop-positions input)))
    (println "Invalid choice.")))
