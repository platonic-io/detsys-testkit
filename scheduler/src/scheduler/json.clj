(ns scheduler.json
  (:refer-clojure :exclude [read])
  (:require [jsonista.core :as j]
            [shams.priority-queue :as pq]))

(set! *warn-on-reflection* true)

(def mapper
  (j/object-mapper
   {:encode-key-fn true
    :decode-key-fn true
    ;; TODO(stevan): When running the native-image it fails serialise the
    ;; agenda, not sure how to get this to work...
    ;; :encoders {shams.priority_queue.PersistentPriorityQueue
               ;; (fn [x jg] (.writeString jg (.toString (vec x))))}
    }))

(defn read
  [s]
  (j/read-value s mapper))

(defn write
  [v]
  (j/write-value-as-string v mapper))
