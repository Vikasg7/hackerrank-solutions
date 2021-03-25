(require 
  '[clojure.string :as S :only [split join replace]])

(def ones ["One" "Two" "Three" "Four" "Five" "Six" "Seven" "Eight" "Nine" "Ten" "Eleven" "Twelve" "Thirteen" "Fourteen" "Fifteen" "Sixteen" "Seventeen" "Eighteen" "Nineteen"])

(def tys ["Twenty" "Thirty" "Forty" "Fifty" "Sixty" "Seventy" "Eighty" "Ninety"])

(defn div? [n d]
  (zero? (mod n d)))

(defn quot-mod [n d]
  [(quot n d) (mod n d)])

(defn convert [n]
  (cond (zero? n)  ""
        (< n 20)   (nth ones (dec n))
        (< n 100)  (let [[t o] (quot-mod n 10)]
                   (str (nth tys (- t 2)) " " (convert o)))
        (< n 1E+3) (let [[h t] (quot-mod n 100)]
                   (str (convert h) " Hundred " (convert t)))
        (< n 1E+6) (let [[t h] (quot-mod n (long 1E+3))]
                   (str (convert t) " Thousand " (convert h)))
        (< n 1E+9) (let [[m t] (quot-mod n (long 1E+6))]
                   (str (convert m) " Million " (convert t)))
        :else      (let [[b m] (quot-mod n (long 1E+9))]
                   (str (convert b) " Billion " (convert m)))))

(defn clean [t]
  (S/replace t #"\s+" " "))

(def number-to-text (comp clean convert))

(defn solve [T & nz]
  (map number-to-text nz))

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