(ns scheduler.agenda
  (:require [clojure.data.priority-map :as pm]
            [clojure.spec.alpha :as s]
            [scheduler.spec :refer [>defn => component-id?]]))

(def command? map?)
(s/def ::command command?)
(s/def ::component-id component-id?)
(def entry? (s/keys :req-un [::command
                             ::component-id]))
(def timestamp? int?)
(def agenda? (s/coll-of (s/tuple entry? timestamp?)))

(>defn empty-agenda
  []
  [=> agenda?]
  (pm/priority-map))

(defn enqueue
  [agenda entry timestamp]
  [agenda? entry? timestamp? => agenda?]
  (conj agenda [entry timestamp]))

(defn dequeue
  [agenda]
  [agenda? => (s/tuple agenda? entry?)]
  [(rest agenda) (first agenda)])

(comment
  (-> (empty)
      (enqueue {:command {:name :a, :parameters []}, :component-id "a"} 3)
      (enqueue {:command {:name :b, :parameters []}, :component-id "b"} 1)
      (enqueue {:command {:name :c, :parameters []}, :component-id "c"} 2)
      (dequeue)))
