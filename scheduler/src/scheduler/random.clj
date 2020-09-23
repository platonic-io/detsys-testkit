(ns scheduler.random
  (:require [clojure.data.generators :as gen]))

(defn exponential
  [mean]
  (let [u (gen/double)]
    (* (- mean) (Math/log u))))

(defn mean
  [xs]
  (/ (reduce + 0 xs) (count xs)))

(comment
  (mean (repeatedly 10000 #(exponential 0.2))) )
