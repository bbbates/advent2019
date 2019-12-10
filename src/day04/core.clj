(ns day04.core)

(def never-decreases (filter (partial apply <=)))

(defn- two-adjacent? [digits]
  (->>
    (partition 2 1 digits)
    (some #(apply = %))))
(def has-two-adjacents (filter two-adjacent?))

(defn- ->digit-vec [n]
  (->> n
       (String/valueOf)
       (mapv #(Integer/parseInt (String/valueOf %)))))
(def digit-vecs (map ->digit-vec))

(def satisfy-criteria
  (comp digit-vecs never-decreases has-two-adjacents))

(assert (= 1 (count (into [] satisfy-criteria [123789 223450 111111]))))

(defn solve-part-one [input-range]
  (->> (into []
             satisfy-criteria
             (apply range input-range))
       (count)))

(defn -main []
  (let [range [172930 (inc 683082)]]
    (println "Part 1>>>" (solve-part-one range))))

;(-main)
