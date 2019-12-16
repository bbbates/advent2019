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

(defn -main []
  (let [image-layers (-> (slurp "src/day08/input.txt")
                         (clojure.string/trim)
                         (parse-layers [25 6]))]
    (println "Part 1>>>>" (solve-part-one image-layers))))

(-main)
