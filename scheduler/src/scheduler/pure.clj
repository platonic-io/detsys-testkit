(ns scheduler.pure
  (:require [clojure.spec.alpha :as s]
            [ghostwheel.core :as g
             :refer [>defn >defn- >fdef => | <- ?]]))

(s/def ::total-executors     pos-int?)
(s/def ::connected-executors pos-int?)
(s/def ::components          (s/coll-of string?))
(s/def ::state               #{:ready :started})

(s/def ::data (s/keys :un-req [::total-executors
                               ::connected-executors
                               ::components
                               ::state]))

(>defn init-data
  []
  [=> ::data]
  {:total-executors     "a"
   :connected-executors 0
   :components          {}
   :state               :started
   })

;; (g/check)
;; (init-data)

(defn state-transition
  [data]
  (update data :state
          (fn [state]
            (case state
              :started (if (= (:connected-executors data) (:total-executors data))
                         :ready
                         :waiting-for-executors)
              state))))

(defn ap
  [data transition output]
  {:data   (transition data)
   :output (output data)})

(defn register-executor
  [data {:keys [executor-id components]}]
  (-> data
      (update :connected-executors inc)
      (update :components #(merge % (apply hash-map
                                           (interleave components
                                                       (replicate (count components)
                                                                  executor-id)))))
      (ap state-transition
            #(- (:total-executors %) (:connected-executors %)))))

;; (register-executor (init-data) {:executor-id "exec", :components ["a" "b"]})
