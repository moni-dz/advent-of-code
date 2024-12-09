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

(defn find-free-spans [blocks]
  (let [n (count blocks)]
    (loop [idx 0
           spans (transient [])
           start nil
           size 0]
      (if (>= idx n)
        (if start
          (persistent! (conj! spans [start size]))
          (persistent! spans))
        (let [is-free (= (blocks idx) \.)]
          (cond
            (and is-free (nil? start))
            (recur (inc idx) spans idx 1)

            (and is-free start)
            (recur (inc idx) spans start (inc size))

            (and (not is-free) start)
            (recur (inc idx) (conj! spans [start size]) nil 0)

            :else
            (recur (inc idx) spans nil 0)))))))

(defn compact-files [blocks]
  (let [files (sort-by first > (find-files blocks))
        spans (find-free-spans blocks)
        blks (vec blocks)]

    (loop [curr-blks blks
           [file & rest-files] files
           curr-spans spans]

      (if (nil? file)
        curr-blks
        (let [[id size start end] file
              suitable-span (first
                             (filter (fn [[s sz]]
                                       (and (< s start) (>= sz size)))
                                     curr-spans))]
          (if suitable-span
            (let [[span-start span-size] suitable-span

                  new-spans (cond-> (filterv #(not= (first %) span-start) curr-spans)
                              (> span-size size)
                              (conj [(+ span-start size) (- span-size size)]))

                  new-spans (conj new-spans [start (- (inc end) start)])

                  new-blks (reduce (fn [b i] (assoc b i \.))
                                   curr-blks
                                   (range start (inc end)))

                  new-blks (reduce (fn [b i] (assoc b i id))
                                   new-blks
                                   (range span-start (+ span-start size)))]

              (recur new-blks rest-files new-spans))
            (recur curr-blks rest-files curr-spans)))))))

(defn checksum [blocks]
  (reduce-kv
   (fn [sum idx blk]
     (if (number? blk)
       (+ sum (* idx blk))
       sum))
   0
   (vec blocks)))

(let [data (-> (slurp "inputs/2024/9.txt")
               create-disk-map)]
  [(time (-> data compact-chunks checksum))
   (time (-> data compact-files checksum))])
