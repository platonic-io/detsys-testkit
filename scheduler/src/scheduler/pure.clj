(ns scheduler.pure
  (:require [clojure.spec.alpha :as s]
            [clojure.data.generators :as gen]
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
   :seed                (java.util.Random. 1)
   :total-commands      0
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
      (update :agenda #(agenda/enqueue % (assoc entry :id (:total-commands data)) timestamp))
      (update :total-commands inc)
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
                  :timestamp timestamp
                  :body (merge entry {:timestamp timestamp})}])
        [data {:more? false}]))))


(defn rand-int-in
  [seed min max]
  (+ min (.nextInt seed max)))

;; we should probably check what is currently in the agenda as well
;; to figure out where to schedule things..
(defn add-commands
  [data timestamp new-commands]
  (let [seed (:seed data)]
    (loop [data data
           commands new-commands ;; if we have bound gen/*rnd* properly this is deterministic
           ]
      (if (empty? commands)
        [data {}] ;; check if agenda is empty and change state?
        (let [extra-time (rand-int-in seed 1 10000)]
          (recur (-> (enqueue-command data {:entry (first commands) ;; pick at random with seed
                                            :timestamp (+ timestamp extra-time)})
                     first)
                 (rest commands)))))))

;; TODO(stevan): move to other module?
(defn execute!
  [data]
  (let [[data' r] (-> data execute)]
    (log/debug :execute r)
    (when (:more? r)
      (log/debug :more?)
      ;; TODO(stevan): Retry on failure, this possibly needs changes to executor
      ;; so that we don't end up executing the same command twice.
      (let [response (client/post (:url r) {:body (json/write (:body r))})
            new-commands (-> response :body json/read)]
        (add-commands data' (:timestamp r) new-commands)))))

(defn set-seeed!
  [data {new-seed :new-seed}]
  [(assoc data :seed (java.util.Random. new-seed)) new-seed])

(defn load-test!
  [data {:keys [test-id]}]
  )

(def danne-test
  (-> (init-data)
      (register-executor {:executor-id "http://localhost:3001" :components ["inc" "store"]})
      first
      (enqueue-command {:entry {:command {:name "do-inc" :parameters []}
                                :component-id "inc"}
                        :timestamp 1})
      first
      (enqueue-command {:entry {:command {:name "do-inc" :parameters []}
                                :component-id "inc"}
                        :timestamp 2})
      first))

(comment
  (-> (init-data)
      (register-executor {:executor-id "http://localhost:3000" :components ["c"]})
      first
      (enqueue-command {:entry {:command {:name "a", :parameters []}
                                :component-id "c"}
                        :timestamp 1})
      first
      execute!))
