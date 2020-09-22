(ns scheduler.agenda
  (:require [clojure.spec.alpha :as s]
            [shams.priority-queue :as pq]
            [scheduler.spec :refer [>defn => component-id?]]))

(set! *warn-on-reflection* true)

(def command? (s/or :string string?
                    :keyword keyword?))
(s/def ::command command?)
(def parameters? map?)
(s/def ::parameters parameters?)
(s/def ::to component-id?)
(s/def ::from string?)
(def timestamp-regex #"^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$")
(comment
  (re-matches timestamp-regex "2000-10-12T20:03:41Z"))
(def timestamp? (s/or :relative nat-int?
                      :absolute #(re-matches timestamp-regex %)))
(s/def ::at timestamp?)
(def entry? (s/keys :req-un [::command
                             ::parameters
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
      (enqueue {:command :a
                :parameters {}
                :to "a"
                :from "client"
                :at 3})
      (enqueue-many [{:command :b
                      :parameters {}
                      :to "b"
                      :from "client"
                      :at 1}
                     {:command :c
                      :parameters {}
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
      (enqueue-many [{:command :a
                      :parameters {}
                      :to "t"
                      :from "client"
                      :at 1}
                     {:command :a
                      :parameters {}
                      :to "t"
                      :from "client"
                      :at 1}])
      ) )
