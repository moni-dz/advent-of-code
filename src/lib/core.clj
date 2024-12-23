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