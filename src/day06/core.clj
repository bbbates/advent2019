(ns day06.core)


(defn- parse-orbit
  [orbit-def]
  (clojure.string/split orbit-def #"\)"))

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

#_(let [orbits (->> (clojure.string/split "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L" #"\n")
                  (map (comp parse-orbit clojure.string/trim)))
      grouped (group-by first orbits)
      com-orbit (get grouped "COM")
      orbit-ctr (fn orbit-ctr [orbits total]
                  (println orbits total)
                  (reduce + total (map #(orbit-ctr (get grouped (second %)) (inc total)) orbits)))]


  (orbit-ctr com-orbit 0)

  #_(solve-part-one orbits))

(defn -main []
  (let [orbits (as-> (slurp "src/day06/input.txt") $
                     (clojure.string/split $ #"\n")
                     (map (comp parse-orbit clojure.string/trim) $))]
    (println "Part 1 >>>>>" (solve-part-one orbits))))
