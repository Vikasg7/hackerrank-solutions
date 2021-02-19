(require 
  '[clojure.string :refer [split join]])

; Generates next fib given the last two even fibs 
(defn even-fib [[f1 f2]]
  (let [nxt (-> (* 4 f2) (+ f1))]
  [f2 nxt]))

(defn even-fibs-upto [l]
  (->> (iterate even-fib [0 2])
       (map second)
       (take-while #(<= % l))))

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