(ns scheduler.json
  (:refer-clojure :exclude [read])
  (:require [jsonista.core :as j])
  (:import [com.fasterxml.jackson.core JsonGenerator]))

(set! *warn-on-reflection* true)

(def mapper
  (j/object-mapper
   {:encode-key-fn true
    :decode-key-fn true
    :encoders {shams.priority_queue.PersistentPriorityQueue
               (fn [^shams.priority_queue.PersistentPriorityQueue pq
                    ^JsonGenerator jg]
                 (.writeString jg (str (.seq pq))))}}))

(defn read
  [s]
  (j/read-value s mapper))

(defn write
  [v]
  (j/write-value-as-string v mapper))
