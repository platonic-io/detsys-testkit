(ns scheduler.pure
  (:require [clojure.spec.alpha :as s]
            [scheduler.spec :refer [>defn => component-id?]]
            [scheduler.agenda :as agenda]))

(defn nat?
  "Check if something is a natural number."
  [x]
  (and (int? x) (<= 0 x)))

(s/def ::total-executors     pos-int?)
(s/def ::connected-executors nat?)
(s/def ::topology            (s/map-of string? string?))
(s/def ::agenda              agenda/agenda?)
(s/def ::state               #{:ready :started :running})

(s/def ::data (s/keys :req-un [::total-executors
                               ::connected-executors
                               ::topology
                               ::agenda
                               ::state]))

(s/def ::components (s/coll-of component-id? :kind vector?))
(s/def ::remaining-executors nat?)

(>defn init-data
  []
  [=> ::data]
  {:total-executors     1
   :connected-executors 0
   :topology            {}
   :agenda              (agenda/empty-agenda)
   :state               :started})

(defn ap
  [data f]
  [data (f data)])

(defn update+
  "Like `update`, but the function also has access to the original map."
  [m k f]
  (update m k #(f m %)))

(s/def ::executor-id string?)
(s/def ::components (s/coll-of string?))

(>defn register-executor
  [data {:keys [executor-id components]}]
  [::data (s/keys :req-un [::executor-id ::components])
   => (s/tuple ::data (s/keys :req-un [::remaining-executors]))]
  (-> data
      (update :connected-executors inc)
      (update :topology #(merge % (apply hash-map
                                         (interleave components
                                                     (replicate (count components)
                                                                executor-id)))))
      (update+ :state
               (fn [data' state]
                 (let [state' (case (compare (:connected-executors data') (:total-executors data'))
                                -1 :waiting-for-executors
                                0  :ready
                                1  :error-too-many-executors)]
                   (case state
                     :started state'
                     :waiting-for-executors state'
                     :error-can't-register-in-this-state))))
      (ap (fn [data']
            {:remaining-executors (- (:total-executors data')
                                     (:connected-executors data'))}))))

(>defn enqueue-command
  [data entry timestamp]
  [::data agenda/entry? agenda/timestamp? => (s/tuple ::data #{:ok})]
  (-> data
      (update :agenda #(agenda/enqueue % entry timestamp))
      (update :state (fn [state]
                       (case state
                         :ready   :running
                         :running :running
                         :error-cannot-enqueue-in-this-state)))
      (ap (constantly :ok))))

(comment
  (-> (init-data)
      (register-executor {:executor-id "e" :components ["c"]})
      first
      (enqueue-command {:command {:name "a", :parameters []}, :component-id "c"} 1)
      first
      (enqueue-command {:command {:name "b", :parameters []}, :component-id "c"} 0)))


(defn status
  [data]
  [data data])
