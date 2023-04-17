(ns org.candelbio.rawsugar.matching
  (:require [clojure.string :as str]
            [org.candelbio.multitool.core :as u])
  )

;;; TODO needs testing

;;; downcase and remove all spaces and punctuation from a string, for matching
;;; TODO global memoization not good for long-term server
(def stripped
  (memoize
   (fn [s]
     (str/lower-case (str/replace s #"[\W_]" "")))))

;;; This is slow (type hinting gave a 100x speedup, but still too slow)
;;; TODO Use a smarter algorithm, possibly Needleman-Wunsch alignment (Rob suggestion)
(defn biggest-shared-substring
  [^String s1 ^String s2]
  (let [c1 (count s1) c2 (count s2)]
    (loop [i 0
           j 0
           maxi 0
           max-match nil]
      (cond (= j c2)
            max-match
            (= i c1)
            (recur 0
                   (+ j 1)
                   maxi
                   max-match)
            (= (.charAt s1 i) (.charAt s2 j))
            (let [match-len
                  (- 
                   (loop [len 1]
                     (if (.regionMatches s1 i s2 j len)
                       (recur (+ 1 len))
                       len)) 1)]
              (recur (+ i 1) j
                     (max maxi match-len)
                     (if (> match-len maxi)
                       (subs s1 i (+ i match-len)) max-match)))
            :else
            (recur (+ i 1)
                   j
                   maxi
                   max-match)))))
      
;;; Should do something about equally-good best (which actually happens in our test data)
(defn best-match
  [string set]
  (u/max-by #(count (biggest-shared-substring (stripped string) (stripped %))) set))

(defn scored-matches
  [string set]
  
  (sort-by (comp - first)
           (map (fn [s] (let [m (biggest-shared-substring (stripped string) (stripped s))]
                          [(count m) m s]))
                set)))

(defn ranked-matches
  [string set]
  (reverse
   (sort-by (comp count first)
            (map (fn [m] [(biggest-shared-substring (stripped string) (stripped m)) m]) set))))

(defn best-match-unambiguous
  [string set threshold]
  (let [ranked (ranked-matches string set)
        maxi (count (ffirst ranked))
        filtered (filter #(= maxi (count (first %))) ranked)]
    (if (and (= (count filtered) 1)
             (or (not threshold) (>= maxi threshold)))
      (second (first ranked))
      nil)))

(defn get-best
  "Akin to get, but
  -  does best-match rather than exact match
  -  returns [match value]"
  [map value]
  (let [match (best-match value (set (keys map)))]
    [match (get map match)]))

(defn get-best-threshold
  "Akin to get, but
  - does best-match rather than exact match
  - if threshold supplied, match has to exceed it
  - returns [matched-key matched-value score]"
  [map value threshold]
  (let [[score _ matched :as best]
        (first (scored-matches value (set (keys map))))]
    (when (and best (or (not threshold) (>= score threshold)))
      [matched (get map matched) score])))


