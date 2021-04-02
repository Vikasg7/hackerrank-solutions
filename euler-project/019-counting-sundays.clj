(require 
  '[clojure.string :as S :only [split join replace trim]])

(defn vector-from [coll]
  (apply vector coll))

(defn partitionv [n coll]
  (->> (partition n coll)
       (mapv vector-from)))

(defn lys-before [y]
  (let [y (dec y)]
  (-> (- (quot y 4)
         (quot y 100))
      (+ (quot y 400)))))

(defn lys-between [s e]
  (cond (> s e) 0
        :else   (- (lys-before (inc e))
                   (lys-before s))))
(defn div? [n d]
  (zero? (mod n d)))

(defn leap? [y]
  (or (div? y 400)
      (and (div? y 4) (not (div? y 100)))))

(defn days
  ([coll]
    (apply days coll))
  ([y m d]
    (let [nd [0 31 59 90 120 151 181 212 243 273 304 334]
          ld [0 31 60 91 121 152 182 213 244 274 305 335]]
    (cond (leap? y) (+ d (nth ld (dec m)))
          :else     (+ d (nth nd (dec m)))))))

(defn days-diff [[y1 m1 d1 :as s] 
                 [y2 m2 d2 :as e]]
  (let [ds (days s)
        de (days e)
        ls (lys-between y1 (dec y2))
        dd (* 365 (- y2 y1))]
  (cond (= y1 y2) (-> (- de ds) (+ 1))
        :else     (-> (+ dd de ls) (- ds) (+ 1)))))

(defn flip 
  ([f]
    (partial flip f))
  ([f & ls]
    (apply f (reverse ls))))

(defn mod-add [m & nums]
  (->> (map #(mod % m) nums)
       (reduce +)
       (flip mod m)))

;; Zellers Congruence
(defn week-day 
  ([coll]
    (apply week-day coll))
  ([y m d]
    (let [dm [6 7 1 2 3 4 5]
          ny (if (#{1 2} m) (dec y) y)
          nm (if-let [s ({1 13 ,2 14} m)] s m)
          k  (mod ny 100)
          j  (quot ny 100)
          wd (mod-add 7 d
                        (-> (+ nm 1) (* 13) (quot 5))
                        k
                        (quot k 4)
                        (quot j 4)
                        (* 5 j))]
    (nth dm wd))))

(defn sunday-count [[s e]]
  (let [wd (week-day s)
        ds (days-diff s e)]
  (->> (- ds 7 wd)
       (flip quot 7))))

(defn sunday-on-fst [[[y1 m1 d1 :as s]
                      [y2 m2 d2 :as e]]]
  (let [gtEq (comp not neg?)
        ltEq (comp not pos?)]
  (for [y (range y1 (inc y2))
        m (range 1 13)
        :let [dt [y m 1]]
        :when (and (gtEq (compare dt s))
                   (ltEq (compare dt e))
                   (= 7 (week-day dt)))]
    dt)))

(defn solve [T & nz]
  (let [tz (->> (partitionv 3 nz)
                (partition 2)
                (mapv sort))]
  (map (comp count sunday-on-fst) tz)))

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