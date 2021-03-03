(require 
  '[clojure.string :refer [split join]])

(defn consecutive-pairs
  ([n xs]
    (consecutive-pairs n (count xs) xs))
  ([n x xs]
    (let [pair #(->> (drop % xs) (take n))]
    (->> (range 0 (- x (dec n)))
         (map pair)))))

(defn solve [n & nz]
  (let [cs    (take n nz)
        [d m] (drop n nz)
        pairs (consecutive-pairs m cs)
        pred? #(= d (apply + %))]
  (->> (filter pred? pairs)
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