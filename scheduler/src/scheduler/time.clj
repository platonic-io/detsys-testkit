(ns scheduler.time
  (:require [java-time :as time]
            [scheduler.spec :refer [>defn =>]]))

(defn instant?
  [x]
  (instance? java.time.Instant x))

(>defn init-clock
  []
  [=> instant?]
  (time/instant 0))

(>defn plus-millis
  [instant ms]
  [instant? double? => instant?]
  (time/plus instant (time/nanos (* ms 1000000))))

(def instant time/instant)

(comment
  (str (plus-millis (init-clock) 18.22324)))
