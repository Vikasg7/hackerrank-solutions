(require 
  '[clojure.string :refer [split join]])

(defn interact [fn]
  (let [joinIfSeq #(if (seq? %) (join "\n" %) %)]
  (->> (fn (slurp *in*))
       (joinIfSeq)
       (println))))

(defn prepare [input]
  (let [words  #(split % #"\s")]
  (->> (words input)
       (map read-string))))

(defn sn [n]
  (-> (+ n 1)
      (* n)
      (/ 2)))

(defn sum-of [multiple n]
  (let [nm (-> (/ n multiple)
               (- 1)
               (Math/ceil)
               (int))]
  (* multiple (sn nm))))

(defn sum-of-multiples [n]
  (-> (+ (sum-of 3 n)
         (sum-of 5 n))
      (- (sum-of 15 n))))

(defn solve [t & nums]
  (map sum-of-multiples nums))

;; (defn main []
;;   (let [program #(->> (prepare %) (apply solve))]
;;   (interact program)))

;; (main)

(defn main [input]
  (->> (prepare input)
       (apply solve)))

(main "2\n10\n100")
(= [23 2318] (main "2\n10\n100"))

;; (println (time (main "3\n10 8 6\n2 9 9")))

;; lein exec ~/clojure-practice/main.clj

;; (defn cartesian [& seqs]
;;   (let [v-original-seqs (vec seqs)
;;         increment (fn [v-seqs]
;;                      (loop [i (dec (count v-seqs)), v-seqs v-seqs]
;;                         (if (= i -1) 
;;                            nil
;;                            (if-let [rst (next (v-seqs i))]
;;                               (assoc v-seqs i rst)
;;                               (recur (dec i) (assoc v-seqs i (v-original-seqs i)))))))
;;         step (fn step [v-seqs]
;;                 (when v-seqs
;;                   (cons (map first v-seqs)
;;                         (lazy-seq (step (increment v-seqs))))))]
;;   (when (every? seq seqs)
;;     (lazy-seq (step v-original-seqs)))))

;; (defn cartesian [as bs]
;;   (for [a as
;;         b bs]
;;     [a b]))

;; (defn digits [n]
;;   (let [toInt #(->> (str %) (read-string))]
;;   (->> (str n)
;;        (map toInt))))

;; (defn index-of [coll x]
;;   (let [idx? #(when (= x (second %)) (first %))]
;;   (->> (map-indexed vector coll)
;;        (some idx?))))

;; (defmacro defn-memo [n & body]
;;   `(def ~n (memoize (fn ~body))))

;; ;; returns each element paired with successive element
;; (defn toPairs [coll]
;;   (zip (butlast coll) (rest coll)))

;; (defn zip [& colls]
;;   (partition (count colls) 
;;              (apply interleave colls)))

;; (defn filterWithIdx [pred coll]
;;   (let [iter (fn [[i acc] pred [f & res]]
;;                 (let [nacc (if (pred i f) (conj acc f) acc)]
;;                 (if (= f nil)
;;                    acc
;;                   (recur [(+ i 1) nacc] pred res))))]
;;   (iter [0 []] pred coll)))

;; (defn filterWithIdx [pred coll]
;;   (let [npred (fn [[i acc] v]
;;                 (let [ni   (+ i 1)
;;                       nacc (if (pred i v) (conj acc v) acc)]
;;                 [ni, nacc]))]
;;   (last (reduce npred [0, []] coll))))