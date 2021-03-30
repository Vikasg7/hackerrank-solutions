(require 
  '[clojure.string :as S :only [split join replace trim]])

(def ones ["" "One" "Two" "Three" "Four" "Five" "Six" "Seven" "Eight" "Nine" "Ten" "Eleven" "Twelve" "Thirteen" "Fourteen" "Fifteen" "Sixteen" "Seventeen" "Eighteen" "Nineteen"])

(def tys ["" "" "Twenty" "Thirty" "Forty" "Fifty" "Sixty" "Seventy" "Eighty" "Ninety"])

(def powers ["" "Thousand" "Million" "Billion" "Trillion"])

(defn div? [n d]
  (zero? (mod n d)))

(defn quot-mod [n d]
  [(quot n d) (mod n d)])

(defn hundred [n]
  (cond (zero? n) ""
        (< n 20)  (nth ones n)
        (< n 100) (let [[t o] (quot-mod n 10)]
                  (str (nth tys t) " " (hundred o)))
        :else     (let [[h t] (quot-mod n 100)]
                  (str (hundred h) " Hundred " (hundred t)))))

(defn clean [t]
  (S/trim (S/replace t #"\s+" " ")))

(defn number-to-words
  ([n] 
    (->> (number-to-words [] 0 n)
         (S/join " ")
         (clean)))
  ([acc i n] 
    (let [[q m] (quot-mod n 1000)
          part  (str (hundred m) " " (nth powers i))
          nacc  (cons part acc)]
    (cond (zero? q) nacc
          (zero? m) (recur acc (inc i) q)
          :else     (recur nacc (inc i) q)))))

(defn solve [T & nz]
  (map number-to-words nz))

(defn safeInt [n]
  (try (Long/parseLong n)
  (catch Exception e n)))

(defn interact [func]
  (let [joinIfSeq #(if (seq? %) (S/join "\n" %) %)]
  (->> (func (slurp *in*))
       (joinIfSeq)
       (println))))

(defn prepare [input]
  (let [words  #(S/split % #"\s")]
  (->> (words input)
       (map safeInt))))

(defn main []
  (let [program #(->> (prepare %) (apply solve))]
  (interact program)))

(main)