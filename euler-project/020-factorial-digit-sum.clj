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

(defn fac-digits
  ([n]
    (fac-digits 1 2 n))
  ([lf s n]
    (->> (range s (inc n))
         (reduce str-mult lf))))

(defn fac-digit-tbl
  ([nz] 
    (->> (sort nz) 
         (reduce fac-digit-tbl [[0 1]])
         (into {})))
  ([[[ln lf] & res :as acc] n]
    (let [nf (fac-digits lf (inc ln) n)]
    (cons [n nf] acc))))

(defn solve [t & nz]
  (let [tbl (fac-digit-tbl nz)]
  (map (comp digit-sum tbl) nz)))

(defn safeInt [n]
  (try (Long/parseLong n)
  (catch Exception e n)))

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