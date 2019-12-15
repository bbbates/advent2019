(ns day06.core)

(defn- count-orbits
  [orbits]
  (let [grouped (group-by first orbits)
        com-orbit (get grouped "COM")
        orbit-ctr (fn orbit-ctr [orbits total]
                    (reduce + total (map #(orbit-ctr (get grouped (second %)) (inc total)) orbits)))]
    (orbit-ctr com-orbit 0)))

(defn- solve-part-one
  [orbits]
  (count-orbits orbits))

(defn- solve-part-two [orbits]
  (let [grouped-by-b (group-by second orbits)
        san-orbit (ffirst (get grouped-by-b "SAN"))
        you-orbit (ffirst (get grouped-by-b "YOU"))
        grouped-by-a (group-by first orbits)
        orbit-ctr (fn orbit-ctr [orbits dir total]
                    (cond
                      (= san-orbit (ffirst orbits)) (dec total)
                      (empty? orbits) nil
                      :else
                      (let [[orbit-grouping next-f] (dir {:up [grouped-by-b first] :down [grouped-by-a second]})
                            branches (map #(orbit-ctr (get orbit-grouping (next-f %)) :down (inc total)) orbits)]
                        (if-let [found-total (first (filter number? branches))]
                          ;; if found in branches, then unroll
                          found-total
                          ;; if not found in branches, then keep traversing up
                          (when (= dir :up)
                            (orbit-ctr (get grouped-by-b (ffirst orbits)) :up (inc total)))))))]
    (dec (orbit-ctr (get grouped-by-b "YOU") :up 0))))

(defn- parse-orbit
  [orbit-def]
  (clojure.string/split orbit-def #"\)"))

(defn -main []
  (let [orbits (as-> (slurp "src/day06/input.txt") $
                     (clojure.string/split $ #"\n")
                     (map (comp parse-orbit clojure.string/trim) $))]
    (println "Part 1 >>>>>" (solve-part-one orbits))
    (println "Part 2 >>>>>" (solve-part-two orbits))))
