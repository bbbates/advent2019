(ns day08.core)

(defn solve-part-one
  [layers]
  (let [[fewest-zero-idx] (->> layers
                               (map-indexed (fn [idx layer]
                                              [idx (count (filter zero? layer))]))
                               (sort-by second)
                               (first))
        fewest-zero-layer (nth layers fewest-zero-idx)]
    (* (count (filter #(= 1 %) fewest-zero-layer))
       (count (filter #(= 2 %) fewest-zero-layer)))))

(defn- parse-layers
  [in [width height]]
  (partition-all (* width height)
                 (map #(Integer/parseInt (str %)) in)))

(def pixels {0 :black 1 :white 2 :transparent})

(defn layerise-pixels
  [stacked-pixels]
  (or (first (drop-while #(= % 2) stacked-pixels))
      2))

(assert (= 0 (layerise-pixels [0 1 2 0])))
(assert (= 1 (layerise-pixels [2 1 2 0])))
(assert (= 1 (layerise-pixels [2 2 1 0])))
(assert (= 0 (layerise-pixels [2 2 2 0])))
(assert (= 2 (layerise-pixels [2 2 2 2])))

(defn solve-part-two
  [layers]
  (let [pixels (->> layers
                    (apply interleave)
                    (partition-all (count layers))
                    (map layerise-pixels)
                    (partition-all 25))]
    (doseq [row pixels]
      (println row))))

(defn -main []
  (let [image-layers (-> (slurp "src/day08/input.txt")
                         (clojure.string/trim)
                         (parse-layers [25 6]))]
    (println "Part 1>>>>" (solve-part-one image-layers))
    (solve-part-two image-layers)))

