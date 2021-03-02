(require 
  '[clojure.string :refer [split join]])

(defn even-fibs
  ([] 
    (even-fibs 2 8))
  ([a b]
    (let [nxt (-> (*' 4 b) (+' a))]
    (lazy-seq (cons a (even-fibs b nxt))))))

(defn even-fibs-upto [l]
  (take-while #(<= % l) (even-fibs)))

(defn solve [t & ls]
  (let [sum #(apply + %)]
  (map (comp sum even-fibs-upto) ls)))

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