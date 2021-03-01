(require 
  '[clojure.string :refer [split join]])

(defn gcd [a b]
  (cond (zero? a) b
        (zero? b) a
        :else     (recur b (mod a b))))

(defn lcm [a b]
  (-> (* a b)
      (/ (gcd a b))))

(defn smallest-multiple [N]
  (->> (range 1 (inc N))
       (reduce lcm)))

(defn solve [T & Nz]
  (map smallest-multiple Nz))

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