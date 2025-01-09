(ns lib.core (:gen-class))

(defn sliding
  ([^long n] (sliding n 1))
  ([^long n ^long step]
   (fn [rf]
     (let [a (java.util.ArrayDeque. n)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (.add a input)
          (if (= n (.size a))
            (let [v (vec (.toArray a))]
              (dotimes [_ step] (.removeFirst a))
              (rf result v))
            result)))))))

(def manhattan
  (memoize #(reduce + (map (comp abs -) % %))))

(defn in-bounds?
  ([[x y] [mx my]] (and (<= 0 x (dec mx)) (<= 0 y (dec my))))
  ([[x y] grid is-coll?]
   (and
    is-coll?
    (<= 0 y (dec (count grid)))
    (<= 0 x (dec (count (first grid)))))))

(defn char-at [grid [x y]]
  (when (and (>= x 0) (>= y 0)
             (< y (count grid))
             (< x (count (first grid))))
    (get-in grid [y x])))
