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
(s/def ::state               #{:ready :started})

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
   :agenda              (agenda/empty)
   :state               :started})

(>defn state-transition
  [data]
  [::data => ::data]
  (update data :state
          (fn [state]
            (case state
              :started (if (= (:connected-executors data) (:total-executors data))
                         :ready
                         :waiting-for-executors)
              state))))

(defn ap
  [data transition output]
  [(transition data)
   (output data)])

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
      (ap state-transition
          (fn [data']
            {:remaining-executors (- (:total-executors data')
                                     (:connected-executors data'))}))))

;; (register-executor (init-data) {:executor-id "exec", :components ["a" "b"]})
