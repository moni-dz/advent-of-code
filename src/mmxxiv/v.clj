(ns mmxxiv.v
  (:require [clojure.string :as str]))

(defn valid-sequence? [rules nums]
  (let [valid? (set nums)
        pos (zipmap nums (range))]
    (every? (fn [[a b]]
              (or (not (and (valid? a) (valid? b)))
                  (< (pos a) (pos b))))
            rules)))

(defn create-graph [rule-pairs nums]
  (let [valid? (set nums)]
    (reduce (fn [g [a b]]
              (if (and (valid? a) (valid? b))
                (-> g
                    (update a (fnil conj #{}) b)
                    (update b (fnil conj #{}) nil))
                g))
            (zipmap nums (repeat #{}))
            rule-pairs)))

(defn topological-sort [graph]
  (loop [result []
         nodes (keys graph)
         edges (update-vals graph #(disj % nil))]
    (if (empty? nodes)
      result
      (if-let [node (first (filter #(empty? (edges %)) nodes))]
        (recur (conj result node)
               (remove #{node} nodes)
               (update-vals edges #(disj % node)))
        (concat result nodes)))))

(defn get-middle [nums]
  (nth nums (quot (count nums) 2)))

(def input
  (let [[order produce] (-> (slurp "inputs/2024/5.txt")
                            (str/split #"\n\n")
                            ((fn [[a b]]
                               [(map #(str/split % #"\|") (str/split a #"\n"))
                                (map #(str/split % #",") (str/split b #"\n"))])))]
    {:rules (set (map (fn [[a b]] [(parse-long a) (parse-long b)]) order))
     :sequences (map #(mapv parse-long %) produce)}))

(let [{:keys [rules sequences]} input
      valid? (partial valid-sequence? rules)
      valid-sequences (filter valid? sequences)
      invalid-sequences (remove valid? sequences)]

  [(reduce + (map get-middle valid-sequences))

   (->> invalid-sequences
        (map #(topological-sort (create-graph rules %)))
        (map get-middle)
        (reduce +))])







