(require 
  '[clojure.string :refer [split join]])

(defn solve [n & ar]
  (let [birdFreq (into [] (frequencies ar))
        bird     first
        freq     second
        maxFreq  (->> (map freq birdFreq)
                      (apply max))
        maxFreq? #(= maxFreq (freq %))]
  (->> (filter maxFreq? birdFreq)
       (map bird)
       (apply min))))

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