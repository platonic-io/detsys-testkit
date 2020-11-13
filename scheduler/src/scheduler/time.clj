(ns scheduler.time
  (:require [scheduler.spec :refer [>defn =>]]))

(set! *warn-on-reflection* true)

(defn instant?
  [x]
  (instance? java.time.Instant x))

(defn before?
  [^java.time.Instant this ^java.time.Instant that]
  (.isBefore this that))

(>defn init-clock ^java.time.Instant
  []
  [=> instant?]
  (java.time.Instant/ofEpochSecond 0))

(defn plus-millis ^java.time.Instant
  [^java.time.Instant instant ^double ms]
  [instant? double? => instant?]
  (.plusNanos instant (* ms 1000000)))

(defn instant
  [s]
  (java.time.Instant/parse s))

(comment
  (before? (init-clock) (java.time.Instant/now))
  (plus-millis (init-clock) 18.22324)
  (instant "1970-10-10T00:00:00Z") )
