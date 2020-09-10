(ns scheduler.agenda
  (:require [clojure.spec.alpha :as s]
            [shams.priority-queue :as pq]
            [scheduler.spec :refer [>defn => component-id?]]))

(set! *warn-on-reflection* true)

(def command? map?)
(s/def ::command command?)
(s/def ::to component-id?)
(s/def ::from string?)
(def timestamp? nat-int?)
(s/def ::at timestamp?)
(def entry? (s/keys :req-un [::command
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
      (enqueue {:command {:name :a, :parameters []}
                :to "a"
                :from "client"
                :at 3})
      (enqueue-many [{:command {:name :b, :parameters []}
                      :to "b"
                      :from "client"
                      :at 1}
                    {:command {:name :c, :parameters []}
                     :to "c"
                     :from "client"
                     :at 2}])
      (dequeue)
      first
      (dequeue)
      first
      (dequeue)
      ) )

(comment
  (-> (empty-agenda)
      (enqueue-many [{:command {:name :a, :parameters []}
                      :to "t"
                      :from "client"
                      :at 1}
                     {:command {:name :a, :parameters []}
                      :to "t"
                      :from "client"
                      :at 1}])
      ) )
