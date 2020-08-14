(ns scheduler.pure
  (:require [clojure.spec.alpha :as s]
            [clj-http.client :as client]
            [scheduler.spec :refer [>defn => component-id?]]
            [scheduler.agenda :as agenda]
            [scheduler.json :as json]
            [taoensso.timbre :as log]))

(defn nat?
  "Check if something is a natural number."
  [x]
  (and (int? x) (<= 0 x)))

(s/def ::total-executors     pos-int?)
(s/def ::connected-executors nat?)
(s/def ::topology            (s/map-of string? string?))
(s/def ::agenda              agenda/agenda?)
(s/def ::state               #{:ready :started :running
                               :error-cannot-register-in-this-state
                               :error-cannot-enqueue-in-this-state
                               :error-cannot-execute-in-this-state})

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
                 (let [state' (case (compare (:connected-executors data')
                                             (:total-executors data'))
                                -1 :waiting-for-executors
                                0  :ready
                                1  :error-too-many-executors)]
                   (case state
                     :started state'
                     :waiting-for-executors state'
                     :error-cannot-register-in-this-state))))
      (ap (fn [data']
            {:remaining-executors (max 0 (- (:total-executors data')
                                            (:connected-executors data')))}))))

(s/def ::queue-size nat?)

(>defn enqueue-command
  [data {:keys [entry timestamp]}]
  [::data (s/keys :req-un [::entry ::timestamp])
   => (s/tuple ::data (s/keys :req-un [::queue-size]))]
  (-> data
      (update :agenda #(agenda/enqueue % entry timestamp))
      (update :state (fn [state]
                       (case state
                         :ready   :running
                         :running :running
                         :error-cannot-enqueue-in-this-state)))
      (ap (fn [data'] {:queue-size (count (:agenda data'))}))))

(defn status
  [data]
  [data data])

(defn execute
  [data]
  (if (not= (:state data) :running)
    [(assoc data :state :error-cannot-execute-in-this-state) nil]
    (let [[agenda' [entry timestamp]] (agenda/dequeue (:agenda data))]
      (if entry
        (let [data' (assoc data :agenda agenda')
              executor-id (get (:topology data) (:component-id entry))]
          ;; TODO(stevan): handle case when executor-id doesn't exist... Perhaps
          ;; this should be checked when commands are enqueued?
          [data' {:more? true
                  :url (str executor-id "/api/command")
                  :body (merge entry {:timestamp timestamp})}])
        [data {:more? false}]))))

;; TODO(stevan): move to other module?
(defn execute!
  [data]
  (let [r (-> data execute second)]
    (log/debug :execute r)
    (when (:more? r)
      (log/debug :more?)
      (client/post (:url r) {:body (json/write (:body r))}))))

(defn load-test!
  [data {:keys [test-id]}]
  )

(comment
  (-> (init-data)
      (register-executor {:executor-id "http://localhost:3000" :components ["c"]})
      first
      (enqueue-command {:entry {:command {:name "a", :parameters []}
                                :component-id "c"}
                        :timestamp 1})
      first
      execute!))
