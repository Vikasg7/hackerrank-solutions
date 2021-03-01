(require 
  '[clojure.string :refer [split join]])

(defn solve [n & ar]
  (let [freqs (vals (frequencies ar))
        pairs #(quot % 2)
        sum   #(apply + %)]
  (sum (map pairs freqs))))

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