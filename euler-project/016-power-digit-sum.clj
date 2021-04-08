(require 
  '[clojure.string :as S :only [split join replace trim]])

(defn debug [n]
  (println n)
  n)

(defn safeInt [n]
  (try (Long/parseLong n)
  (catch Exception e n)))

(defn digits [n]
  (let [toInt #(->> (str %) (safeInt))]
  (map toInt (str n))))

(defn zeroIfEmpty [ls]
  (if (empty? ls) (list 0) ls))

(defn byCarry [[lc & prv] n]
  (let [nn (+ n lc)
        cur (mod nn 10)
        nc  (quot nn 10)]
  (conj prv cur nc)))

(defn add-products [cnt ls]
  (cond (< cnt 2) (first ls)
        :else     (reduce (partial mapv +) ls)))

(defn mult [ls n]
  (cond (zero? n) nil
        (= n 1)   ls
        :else     (mapv #(* % n) ls)))

(defn zeros [t i n]
  (when (some? n)
    (concat (repeat i 0) n (repeat (- t i) 0))))

(defn add-zeros [t ls]
  (let [t (dec t)]
  (keep-indexed (partial zeros t) ls)))

(defn concat-with [suffix prefix]
  (concat prefix suffix))

(defn list-mult [a b]
  (let [rev      (map reverse [a b])
        [as bs]  (map #(drop-while zero? %) rev)
        tZeros   (mapcat #(take-while zero? %) rev)
        products (map #(mult as %) bs)
        pCnt     (count bs)]
  (->> (add-zeros pCnt products)
       (add-products pCnt)
       (reduce byCarry [0])
       (concat-with tZeros)
       (drop-while zero?)
       (zeroIfEmpty))))

(defn str-mult [& ls]
  (->> (map digits ls)
       (reduce list-mult)
       (reduce str)))

(defn strip-zeros [t]
  (S/replace t #"0+$" ""))

(defn digit-sum [t]
  (->> (digits t)
       (reduce + 0)))

(defn to-base [b n]
  (Integer/toString n 2))

(defn bin-exp [n e]
  (let [be (to-base 2 e)
        pred (fn [r b]
                (case b
                   \1 (str-mult (str-mult r r) n)
                   \0 (str-mult r r)))]
  (reduce pred 1 be)))

(defn sum-tbl 
  ([nz] 
    (->> (dedupe (sort nz))
         (reduce sum-tbl [[0 1]])
         (into {})))
  ([[[ln lp] & res :as tbl] n]
    (let [np (str-mult lp (bin-exp 2 (- n ln)))]
    (cons [n np] tbl))))

(defn solve [T & nz]
  (let [tbl (sum-tbl nz)]
  (map (comp digit-sum tbl) nz)))

(defn interact [func]
  (let [joinIfSeq #(if (seq? %) (S/join "\n" %) %)]
  (->> (func (slurp *in*))
       (joinIfSeq)
       (println))))

(defn prepare [input]
  (let [words  #(S/split % #"\s+")]
  (->> (words input)
       (map safeInt))))

(defn main []
  (let [program #(->> (prepare %) (apply solve))]
  (interact program)))

(main)