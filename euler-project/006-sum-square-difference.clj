(require 
  '[clojure.string :refer [split join]])

(defn sum-of-series [n]
  (-> (* n (+ n 1)) 
      (/ 2)))

(defn sum-of-squares [n]
  (-> (* n (+ n 1) (+ (* 2 n) 1))
      (/ 6)))

(defn square [n]
  (* n n))

(defn sum-square-diff [N]
  (- (square (sum-of-series N)) 
     (sum-of-squares N)))

(defn solve [T & Nz]
  (map sum-square-diff Nz))

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