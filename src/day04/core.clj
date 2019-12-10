(ns day04.core
  (:require [taoensso.timbre :as t]))

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

(def satisfy-part1-criteria
  (comp digit-vecs never-decreases has-two-adjacents))

(assert (= 1 (count (into [] satisfy-part1-criteria [123789 223450 111111]))))

(defn solve-part-one [input-range]
  (->> (into []
             satisfy-part1-criteria
             (apply range input-range))
       (count)))

(defn- two-exclusive-adjacent? [digits]
  (let [[sames curr] (reduce
                         (fn [[sames curr] digit]
                           (if (not= (first curr) digit)
                             [(conj sames curr) [digit]]
                             [sames (conj curr digit)]))
                         [[] (vector (first digits))] (rest digits))]
    (some #(= (count %) 2) (conj sames curr))))

(assert (two-exclusive-adjacent? (->digit-vec 112233)))
(assert (not (two-exclusive-adjacent? (->digit-vec 123444))))
(assert (two-exclusive-adjacent? (->digit-vec 111122)))

(def has-two-exclusive-adjacents (filter two-exclusive-adjacent?))

(def satisfy-part2-criteria
  (comp digit-vecs never-decreases has-two-exclusive-adjacents))

(defn solve-part-two [input-range]
  (->> (into []
             satisfy-part2-criteria
             (apply range input-range))
       (count)))

(defn -main []
  (let [range [172930 (inc 683082)]]
    (println "Part 1>>>" (solve-part-one range))
    (println "Part 2>>>" (solve-part-two range))))
