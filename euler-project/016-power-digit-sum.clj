(require 
  '[clojure.string :refer [split join]])

(defn debug [n]
  (println n)
  n)

(defn safeInt [n]
  (try (Integer/parseInt n)
  (catch Exception e n)))

(defn digits [n]
  (let [toInt #(safeInt (str %))]
  (map toInt (str n))))

(defn to-base [b n]
  (Integer/toString n 2))

(defn bin-exp [n e]
  (let [be (to-base 2 e)
        pred (fn [r b]
                (case b
                   \1 (*' (*' r r) n)
                   \0 (*' r r)))]
  (reduce pred 1 be)))

(defn sum-tbl [nz]
  (let [pred (fn [[[ln lp] & res :as tbl] n]
                (let [np (*' lp (bin-exp 2M (- n ln)))]
                (conj tbl [n np])))]
  (->> (dedupe (sort nz))
       (reduce pred [[0 1]])
       (into {}))))

(defn digit-sum [n]
  (apply + (digits n)))

(defn power-digit-sum [tbl n]
  (digit-sum (get tbl n)))

(defn solve [T & nz]
  (let [tbl (sum-tbl nz)]
  (map (partial power-digit-sum tbl) nz)))

(defn interact [func]
  (let [joinIfSeq #(if (seq? %) (join "\n" %) %)]
  (->> (func (slurp *in*))
       (joinIfSeq)
       (println))))

(defn prepare [input]
  (let [words  #(split % #"\s")]
  (->> (words input)
       (map safeInt))))

(defn main []
  (let [program #(->> (prepare %) (apply solve))]
  (interact program)))

;; (time (main))
(main)