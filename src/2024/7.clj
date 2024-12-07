(require '[clojure.string :as str]
         '[clojure.math :as math]
         '[clojure.core.async :as a :refer [>!! <!!]])

(defn evaluate [target exprs ops]
  (reduce (fn [result [op n]]
            (let [new-result
                  (case op
                    \+ (+ result n)
                    \* (* result n)
                    \| (+ (* result (long (math/pow 10 (count (str n))))) n))]
              (if (> new-result target)
                (reduced nil)
                new-result)))
          (first exprs)
          (map vector ops (rest exprs))))

(defn place-ops [n ops]
  (for [i (range (int (math/pow (count ops) n)))]
    (map #(nth ops
               (int (mod (quot i (int (math/pow (count ops) %)))
                         (count ops))))
         (range n))))

(defn solve [ops [target exprs]]
  (some #(when-let [result (evaluate target exprs %)]
           (= target result))
        (place-ops (dec (count exprs)) ops)))

(defn process-chunk [ops chunk]
  (->> chunk
       (filter (partial solve ops))
       (transduce (map first) + 0)))

(defn parallel-solve [data ops]
  (let [n-cores (.. Runtime getRuntime availableProcessors)
        chunks (partition-all (/ (count data) n-cores) data)
        result-chan (a/chan)
        work-done (atom 0)]

    (doseq [chunk chunks]
      (a/thread
        (let [result (process-chunk ops chunk)]
          (>!! result-chan result)
          (swap! work-done inc)
          (when (= @work-done (count chunks))
            (a/close! result-chan)))))

    (loop [sum 0]
      (if-let [value (<!! result-chan)]
        (recur (+ sum value))
        sum))))

(time
 (let [data (->> (slurp "inputs/2024/7.txt")
                 str/split-lines
                 (map #(let [[n rest] (str/split % #":\s+")
                             target (parse-long n)
                             exprs (map parse-long (str/split rest #"\s+"))]
                         [target exprs])))]

   [(parallel-solve data [\+ \*])
    (parallel-solve data [\+ \* \|])]))
