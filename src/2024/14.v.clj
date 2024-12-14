(import '[java.awt.image BufferedImage]
        '[java.awt Color]
        '[javax.imageio ImageIO]
        '[java.io File])

(defn parse-input [s]
  (->> (re-seq #"-?\d+" s)
       (map parse-long)
       (partition 4)
       (map (fn [[x y vx vy]] {:pos [x y] :vel [vx vy]}))))

(defn pos-at [t {[x y] :pos [vx vy] :vel} [w h]]
  [(mod (+ x (* t vx)) w)
   (mod (+ y (* t vy)) h)])

(defn create-image [positions1 positions2 [w h]]
  (let [scale 8
        img (BufferedImage. (* 2 w scale) (* h scale) BufferedImage/TYPE_INT_RGB)
        g (.createGraphics img)
        set1 (set positions1)
        set2 (set positions2)]
    (.setColor g Color/DARK_GRAY)
    (.fillRect g 0 0 (* 2 w scale) (* h scale))
    (.setColor g Color/WHITE)
    (doseq [[x y] set1]
      (.fillRect g (* x scale) (* y scale) scale scale))
    (doseq [[x y] set2]
      (.fillRect g (+ (* w scale) (* x scale)) (* y scale) scale scale))
    (.dispose g)
    img))

(defn save-image [^BufferedImage img frame-number]
  (let [dir (File. "images")]
    (.mkdirs dir)
    (ImageIO/write img "png" (File. dir (format "frame_%04d.png" frame-number)))))

(let [robots (parse-input (slurp "inputs/2024/14.txt"))
      dims [101 103]
      start-time 0
      end-time 6378]
  (println "Saving frames" start-time "to" end-time)
  (doseq [t (range start-time end-time 2)]
    (println "Saving frame" t)
    (-> (create-image
         (map #(pos-at t % dims) robots)
         (map #(pos-at (inc t) % dims) robots)
         dims)
        (save-image t)))
  (println "Done! Images saved in ./images/"))
