(def inputmap [{:a 10 :b 5} {:a 10 :b 7} {:a 9 :b 10} {:a 8 :b 10 :c 5}])

['(:a :b) '(:a :b :c)]

(defn check-val [m key val]
  (= (key m) val))

(map (fn [m] (check-val m :a 10)) inputmap)

(reduce (fn [acc m] (if (check-val m :b 10)
                      (conj acc (keys m))
                      acc))
        []
        inputmap)

(apply check-val inputmap)

;select max salary .

;; sort 3 colors in a vector
(defn swap [v i1 i2] 
  (assoc v i2 (v i1) i1 (v i2)))


;;
;;; 0 - (j - 1) are 0s
;; j - (i - 1) are 1s
;; i - (k - 1) are unknowns
;; k - (len - 1) are 2s
;;
(defn sortcolor-un [inp]
  (letfn [(sfh [i j k sofar]
            (if (>= i k)
              sofar
              (cond (== (nth sofar i) 0) (recur (inc i) (inc j) k (swap sofar i j))
                    (== (nth sofar i) 1) (recur (inc i) j k sofar)
                    (== (nth sofar i) 2) (recur i j (dec k) (swap sofar i (dec k))))))]
    (sfh 0 0 (count inp) inp)))

(sortcolor-un [1 0 0 1 1 1 2 0 1 0 1 0 2])


;; Merge intervals
(defn mergeinteral [inp]
  (let [inp (sort-by #(first %) inp)]
    (reduce (fn [res [st end]]
              (let [[stc endc] (peek res)]
                (if (>= endc st)
                  (conj (pop res) [stc (max end endc)])
                  (conj res [st end]))))
            [(first inp)]
            (rest inp))))

(mergeinteral [[1 3] [2 6] [8 10] [15 18] [1 3] [2 6] [8 10] [15 18]])

;; Increasing triplet
(defn presum [inp act start]
  (reduce (fn [acc e] 
            (if (act e (peek acc))
              (conj acc e)
              (conj acc (peek acc))))
          start
          inp))

(defn incr-trip [inp]
  (let [revmaxs (rseq (presum (rseq inp) > [(peek inp)]))
        mins (presum inp < [(first inp)])]
    (some identity (map #(< %1 %2 %3) mins inp revmaxs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Combination sum       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[2 3 5]
(def target 8)

;;   6 (append 2)   +   5 (append 3)   +   3 (append 5)
;;   
;;   stair case problem:
;;   [1 2] target n
;;   ways(n) = ways(n-1) + ways(n-2) 

(defn into-set [acc coll]
  (if (some #{(frequencies coll)} (map frequencies acc)) acc (conj acc coll)))

(defn combisum [inp targ]
  (cond
    (<= targ 0) []
    :else (reduce
           (fn [acc [ntar n]]
             (reduce into-set acc (map (fn [r] (conj r n)) (combisum inp ntar))))
           (if (some #{targ} inp) [[targ]] [])
           (map (fn [n] [(- targ n) n]) inp))))

(combisum [1 3 5] 8)

(defn combisum-tail [inp targ]
  (letfn [(csh [acc in]
            (cond
              (> (map + in) targ) acc
              (and (= 1 (count in)) (= (map + in) targ)) (into-set acc in)
              (recur (conj acc ))))]))
(cond
  (<= targ 0) []
  :else (reduce
         (fn [acc [ntar n]]
           (reduce into-set acc (map (fn [r] (conj r n)) (combisum inp ntar))))
         (if (some #{targ} inp) [[targ]] [])
         (map (fn [n] [(- targ n) n]) inp)))
(defn fibo [n] 
  (cond 
    (== n 0) 0
    (== n 1) 1
    :else (+ (fibo (- n 1))
             (fibo (- n 2)))))


(defn fibo-tail [n] 
  (letfn [(fibo-h [itr prfib prprfib]
            (cond 
              (== itr n) (+ prfib prprfib)
              :else (recur (inc itr) (+ prfib prprfib) prfib)))]
    (if (< n 2) n (fibo-h 2 1 0))))

(fibo-tail 0)
(fibo 10)

(defn fibo-tail-up [n] 
  (letfn [(fibo-h [itr prprfib prfib]
            (if (> itr 0)
              (recur (dec itr) prfib (+ prfib prprfib))
              prprfib))]
    (fibo-h n 0 1)))


(defn fibo-tail-up-iter [n] 
  (letfn [(fibo-h [n a b]
            (cond 
              (== n 0) a
              (== n 1) b
              :else (recur (dec n) b (+ a b))))]
    (fibo-h n 0 1)))

(fibo-tail-up-iter 3)
;;(count (combisum [1 5 10] 30))

(=  (sort (into [] (map #(into [] (sort %)) (combisum [1 5 10] 30))))
    (sort (into [] (map #(into [] (sort %)) [[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5],[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,5],[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,10],[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,5,5],[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,10],[1,1,1,1,1,1,1,1,1,1,5,5,5,5],[1,1,1,1,1,1,1,1,1,1,5,5,10],[1,1,1,1,1,1,1,1,1,1,10,10],[1,1,1,1,1,5,5,5,5,5],[1,1,1,1,1,5,5,5,10],[1,1,1,1,1,5,10,10],[5,5,5,5,5,5],[5,5,5,5,10],[5,5,10,10],[10,10,10]]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Group anagrams
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn group-anagrams [inp]
  (vals (group-by frequencies inp)))

(def group-anagrams-input ["eat","tea","tan","ate","nat","bat"])


(group-anagrams group-anagrams-input)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     Jump game
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn jump-game [inp]
  (reduce (fn [acc-dis [i step]] 
            (cond (> i acc-dis) (reduced 0)
                  (>= acc-dis (count inp)) (reduced 1)
                  :else (max acc-dis (+ i step))))
          0
          (map vector (range) inp)))


(map vector [1 3 4] [3 4 5])
(reduce (fn [acc n] (if (> acc 5) (reduced acc) (+ acc n))) [1 100 5 6 8])
(jump-game [2,3,1,1,4])
(jump-game [3,2,1,0,4])
(jump-game [2 5 0 0])

(count [1 3 4])
(subvec (subvec [1 3 4] 1) 1)

;; well formed parans
(defn form-paran [n]
  (letfn [(fph [open close acc cstr]
            (if (== (* 2 n) (count cstr)) (conj acc cstr)
                (do
                  (if (< open n) (fph (inc open) close acc (str cstr "(")))
                  (if (< close open) (recur open (inc close) acc (str cstr ")"))))))]
    (fph 0 0 [] "")))

(form-paran 8)
