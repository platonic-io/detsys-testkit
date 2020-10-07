(ns scheduler.agenda
  (:require [clojure.spec.alpha :as s]
            [shams.priority-queue :as pq]
            [scheduler.spec :refer [>defn => component-id?]]
            [scheduler.time :as time]))

(set! *warn-on-reflection* true)

(s/def ::kind string?)
(def event? (s/or :string string?
                  :keyword keyword?))
(s/def ::event event?)
(def args? map?)
(s/def ::args args?)
(s/def ::to component-id?)
(s/def ::from string?)
(s/def ::at time/instant?)
(def entry? (s/keys :req-un [::kind
                             ::event
                             ::args
                             ::to
                             ::from
                             ::at]))
(def agenda? (s/coll-of entry?))

(>defn empty-agenda
  []
  [=> agenda?]
  (pq/priority-queue
   :at
   :elements []
   :priority-comparator compare
   :variant :queue))

(>defn enqueue
  [agenda entry]
  [agenda? entry? => agenda?]
  (conj agenda entry))

(>defn enqueue-many
  [agenda entries]
  [agenda? (s/coll-of entry?) => agenda?]
  (reduce enqueue agenda entries))

(>defn dequeue
  [agenda]
  [agenda? => (s/tuple agenda? (s/nilable entry?))]
  [(pop agenda) (peek agenda)])

(comment
  (-> (empty-agenda)
      (enqueue {:kind "invoke"
                :event :a
                :args {:p 1}
                :to "a"
                :from "client"
                :at (time/instant 3)})
      (enqueue-many [{:kind "invoke"
                      :event :b
                      :args {:q 2}
                      :to "b"
                      :from "client"
                      :at (time/instant 1)}
                     {:kind "invoke"
                      :event :c
                      :args {:q 2}
                      :to "c"
                      :from "client"
                      :at (time/instant 2)}])
      (dequeue)
      first
      (dequeue)
      first
      (dequeue)
      ) )

(comment
  (-> (empty-agenda)
      (enqueue-many [{:kind "invoke"
                      :event :a
                      :args {}
                      :to "t"
                      :from "client"
                      :at (time/instant 1)}
                     {:kind "invoke"
                      :event :a
                      :args {}
                      :to "t"
                      :from "client"
                      :at (time/instant 1)}])
      ) )
