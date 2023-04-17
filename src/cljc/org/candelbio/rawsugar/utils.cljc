(ns org.candelbio.rawsugar.utils
  (:require [inflections.core :as inflect]
            [clojure.string :as s]))

(defn pluralize [count & args]
  (apply
   inflect/pluralize
   (if (nil? count) "no" count)
   args))

(defn pluralize-if
  [n label]
  (when (and n (not (zero? n)))
    (inflect/pluralize n label)))

;;; Like inflect/titleize without the upper casing
(defn modest-titleize
  "Convert `s` into a title."
  [s]
  (when s
    (s/join " " (s/split (name s) #"[-_./ ]"))))

(defn trim-login
  [l]
  (when l
    (let [[match? name] (re-matches #"(.*)@.+" l)]
      (if match?
        name
        l))))

(defn comma-list
  [l]
  (s/join ", " (map str (filter identity l))))

