(require 
  '[clojure.string :refer [split join]])

(defn cartesian [[a & bs]]
  (for [b bs] [a b]))

(defn make-pairs [ar n]
  (->> (range 0 (dec n))
       (map #(drop % ar))
       (mapcat cartesian)))

(defn solve [n k & ar]
  (let [div-by #(zero? (mod %2 %1))
        pred?  #(div-by k (apply + %))]
  (->> (make-pairs ar n)
       (filter pred?)
       (count))))

(defn interact [func]
  (let [joinIfSeq #(if (seq? %) (join "\n" %) %)]
  (->> (func (slurp *in*))
       (joinIfSeq)
       (println))))

(defn prepare [input]
  (let [words  #(split % #"\s")]
  (->> (words input)
       (map read-string))))

(defn main []
  (let [program #(->> (prepare %) (apply solve))]
  (interact program)))

(main)