(ns mmxxiv.ix)

(defn create-disk-map [map-str]
  (loop [digits (seq map-str)
         is-file true
         file-id 0
         blocks []]
    (if (empty? digits)
      blocks
      (let [len (parse-long (str (first digits)))]
        (recur (rest digits)
               (not is-file)
               (if is-file (inc file-id) file-id)
               (into blocks (repeat len (if is-file file-id \.))))))))

(defn compact-chunks [blocks]
  (loop [blks blocks
         left 0
         right (dec (count blocks))]
    (if (>= left right)
      blks
      (cond
        (not= (blks left) \.)
        (recur blks (inc left) right)

        (= (blks right) \.)
        (recur blks left (dec right))

        :else
        (recur (-> blks
                   (assoc left (blks right))
                   (assoc right \.))
               (inc left)
               (dec right))))))

(defn find-files [blocks]
  (let [n (count blocks)]
    (loop [idx 0
           files (transient [])
           curr-id nil
           start nil
           size 0]
      (if (>= idx n)
        (if curr-id
          (persistent! (conj! files [curr-id size start (dec idx)]))
          (persistent! files))

        (let [blk (blocks idx)]
          (if (number? blk)
            (if (and curr-id (= blk curr-id))
              (recur (inc idx) files curr-id start (inc size))
              (let [new-files (if curr-id
                                (conj! files [curr-id size start (dec idx)])
                                files)]
                (recur (inc idx) new-files blk idx 1)))
            (if curr-id
              (recur (inc idx)
                     (conj! files [curr-id size start (dec idx)])
                     nil nil 0)
              (recur (inc idx) files nil nil 0))))))))

(defn find-free-space [blocks start size]
  (loop [pos start
         n 0]
    (cond
      (= n size) start
      (>= pos (count blocks)) nil
      (= (blocks pos) \.) (recur (inc pos) (inc n))
      :else nil)))

(defn find-suitable-position [blocks positions size start max-pos]
  (loop [pos (positions (dec size))]
    (cond
      (>= pos start) nil
      (>= pos max-pos) nil
      :else
      (let [free-space (find-free-space blocks pos size)]
        (if free-space
          free-space
          (recur (inc pos)))))))

(defn update-blocks [blocks start end id pos size]
  (-> blocks
      (as-> b (reduce #(assoc %1 %2 \.) b (range start (inc end))))
      (as-> b (reduce #(assoc %1 %2 id) b (range pos (+ pos size))))))

(defn compact-files [blocks]
  (let [block-count (count blocks)
        positions (transient (vec (repeat 10 0)))
        sorted-files (->> (find-files blocks)
                          (sort-by #(nth % 2) >))]

    (loop [blocks (vec blocks)
           files sorted-files
           current positions
           min-movable-size 10]

      (if (empty? files)
        blocks

        (let [[id size start end] (first files)]
          (if (>= size min-movable-size)
            (recur blocks (rest files) current min-movable-size)

            (let [suitable (find-suitable-position blocks current size start block-count)]
              (if suitable
                (let [new-blocks (update-blocks blocks start end id suitable size)
                      new-positions (assoc! current (dec size) (+ suitable size))]
                  (recur new-blocks (rest files) new-positions min-movable-size))

                (let [new-positions (assoc! current (dec size) start)]
                  (recur blocks (rest files) new-positions size))))))))))

(defn checksum [blocks]
  (reduce-kv
   (fn [sum idx blk]
     (if (number? blk)
       (+ sum (* idx blk))
       sum))
   0
   (vec blocks)))

(let [data (-> (slurp "inputs/2024/9.evil.txt")
               create-disk-map)]
  [(-> data compact-chunks checksum)
   (-> data compact-files checksum)])
