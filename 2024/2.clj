(require '[clojure.string :as str])

(defn valid-diffs? [diffs]
  (and (or (every? pos? diffs) (every? neg? diffs))
       (every? #(<= 1 (abs %) 3) diffs)))

(defn valid-seq? [nums]
  (valid-diffs? (map - (rest nums) nums)))

(defn with-removal? [nums]
  (or (valid-seq? nums)
      (some valid-seq? 
            (for [i (range (count nums))]
              (concat (take i nums) (drop (inc i) nums))))))

(let
   [data (->> (slurp "2.txt")
              str/split-lines
              (mapv #(mapv parse-long (str/split % #"\s+"))))]
   [(count (filter valid-seq? data)) (count (filter with-removal? data))]) ; [526 566]
