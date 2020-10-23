(ns scheduler.pure
  (:refer-clojure :exclude [run!])
  (:require [clojure.spec.alpha :as s]
            [clojure.data.generators :as gen]
            [clj-http.client :as client]
            [clj-http.fake :as fake]
            [scheduler.spec :refer [>defn => component-id?]]
            [scheduler.agenda :as agenda]
            [scheduler.db :as db]
            [scheduler.json :as json]
            [scheduler.random :as random]
            [scheduler.time :as time]
            [taoensso.timbre :as log]))

(set! *warn-on-reflection* true)

(s/def ::total-executors     pos-int?)
(s/def ::connected-executors nat-int?)
(s/def ::topology            (s/map-of string? string?))
(s/def ::agenda              agenda/agenda?)
(s/def ::clock               time/instant?)
(s/def ::state               #{:started
                               :test-prepared
                               :executors-prepared
                               :ready
                               :requesting
                               :responding
                               :finished
                               :error-cannot-load-test-in-this-state
                               :error-cannot-register-in-this-state
                               :error-cannot-create-run-in-this-state
                               :error-cannot-enqueue-in-this-state
                               :error-cannot-execute-in-this-state})

(s/def ::data (s/keys :req-un [::total-executors
                               ::connected-executors
                               ::topology
                               ::agenda
                               ::clock
                               ::faults
                               ::state]))

(s/def ::components (s/coll-of component-id? :kind vector?))
(s/def ::remaining-executors nat-int?)

(>defn init-data
  []
  [=> ::data]
  {:total-executors     1
   :connected-executors 0
   :topology            {}
   :agenda              (agenda/empty-agenda)
   :seed                1
   :faults              []
   :clock               (time/init-clock)
   :state               :started})

(defn ap
  [data f]
  [data (f data)])

(s/def ::test-id nat-int?)
(s/def ::queue-size nat-int?)

;; (if-let [{:keys [message value]} (:error {:error {:message :cannot-load-test-in-this-state, :value :test-prepared}})]
;;  [message value])

(defn precondition
  [data event]
  (case event
    :load-test (if (= (:state data) :started)
                 :ok
                 :error-cannot-load-test-in-this-state)))

;; TODO(stevan): test needs to contain executor topology, so we know how many
;; executors to wait for.
(>defn load-test!
  [data {:keys [test-id]}]
  [::data (s/keys :req-un [::test-id])
   => (s/tuple ::data (s/keys :req-un [::queue-size]))]
  (-> data
      (update :agenda #(agenda/enqueue-many % (db/load-test! test-id)))
      (update :state (fn [state]
                       (case state
                         :started :test-prepared
                         :error-cannot-load-test-in-this-state)))
      (ap (fn [data']
            {:queue-size (count (:agenda data'))}))))

(defn update+
  "Like `update`, but the function also has access to the original map."
  [m k f]
  (update m k #(f m %)))

(s/def ::executor-id string?)
(s/def ::components (s/coll-of string? :kind vector?))

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
                                0  :executors-prepared
                                1  :error-too-many-executors)]
                   (case state
                     :test-prepared state'
                     :waiting-for-executors state'
                     :error-cannot-register-in-this-state))))
      (ap (fn [data']
            {:remaining-executors (max 0 (- (:total-executors data')
                                            (:connected-executors data')))}))))

(comment
  (let [executor-id "http://localhost:3001"
        components ["components0"]]
    (apply hash-map
           (interleave components
                       (replicate (count components)
                                  executor-id))))
  )

(defn create-run!
  [data {:keys [test-id]}]
  (case (:state data)
    :executors-prepared
    (let [run-id (db/create-run! test-id (:seed data))]
      (log/info :run-id run-id)
      [(-> data
           (assoc :state :ready
                  :test-id test-id
                  :run-id (:run-id run-id)))
       run-id])
    [(assoc data :state :error-cannot-create-run-in-this-state) nil]))

(comment
  (-> (init-data)
      (load-test! {:test-id 1})
      first
      (register-executor {:executor-id "http://localhost:3000" :components ["component0"]})
      first
      (create-run! {:test-id 1})) )

(defn should-drop?
  [data entry]
  (let [faults (:faults data)
        entry' (-> entry
                   (select-keys [:to :from :at])
                   (assoc :kind "omission")
                   (update :at str))]
    ((set faults) entry')))

;; TODO(stevan): check if agenda is empty...
(defn execute
  [data]
  (if-not (contains? #{:ready :requesting} (:state data))
    [(assoc data :state :error-cannot-execute-in-this-state) nil]
    (loop [agenda (:agenda data)]
      (let [[agenda' entry] (agenda/dequeue agenda)
            data' (-> data
                      (assoc :agenda agenda')
                      (update :state (fn [state]
                                       (case state
                                         :ready :responding
                                         :requesting :responding
                                         :error-cannot-execute-in-this-state))))
            ;; TODO(stevan): handle case when executor-id doesn't exist... Perhaps
            ;; this should be checked when commands are enqueued?
            executor-id (get (:topology data') (:to entry))]
        (if (should-drop? data entry)
          (recur agenda')
          (do
            (assert executor-id (str "Target `" (:to entry) "' isn't in topology."))
            [data' {:url executor-id
                    :timestamp (:at entry)
                    :body entry}]))))))

(comment
  (-> (init-data)
      (update
       :agenda
       #(agenda/enqueue
         %
         {:kind "invoke", :event :a, :args {}, :at (time/instant 0), :from "f", :to "t"}))
      :agenda
      agenda/dequeue) )

(comment
  (-> (init-data)
      (load-test! {:test-id 1})
      first
      (register-executor {:executor-id "http://localhost:3000/api/v1/event"
                          :components ["node1" "node2"]})
      first
      (create-run! {:test-id 1})
      first
      (execute)) )

(s/def ::kind string?)
(s/def ::event string?)
(s/def ::args map?)
(s/def ::to string?)
(s/def ::from string?)

(def entry? (s/keys :req-un [::kind
                             ::event
                             ::args
                             ::to
                             ::from]))

(def entries? (s/coll-of entry? :kind vector?))

(s/def ::events entries?)

(defn error-state?
  [state]
  (some? (re-matches #"^error-.*$" (name state))))

(defn partition-haskell
  "Effectively though non-lazily splits the `coll`ection using `pred`,
  essentially like `[(filter coll pred) (remove coll pred)]`"
  [pred coll]
  (let [match (transient [])
        no-match (transient [])]
    (doseq [v coll]
      (if (pred v)
        (conj! match v)
        (conj! no-match v)))
    [(persistent! match) (persistent! no-match)]))

(comment
  (partition-haskell odd? (range 10))
  (partition-haskell odd? [])
  )

(>defn parse-client-id
  [s]
  [string? => nat-int?]
  (->> s (re-matches #"^client:(\d+)$") second Integer/parseInt))

(comment
  (parse-client-id "client:0") )

;; TODO(stevan): move to other module, since isn't pure?
;; TODO(stevan): can we avoid using nilable here? Return `Error + Data *
;; Response` instead of always `Data * Response`? Or `Data * Error + Data * Response`?
(>defn execute!
  [data]
  [::data => (s/tuple ::data (s/nilable (s/keys :req-un [::events])))]
  (let [[data' {:keys [url timestamp body]}] (execute data)]
    (if (error-state? (:state data'))
      [data' nil]
      ;; TODO(stevan): Retry on failure, this possibly needs changes to executor
      ;; so that we don't end up executing the same command twice.
      (let [events (-> (client/post url {:body (json/write body)
                                         :content-type "application/json; charset=utf-8"})
                          :body
                          json/read)]

        ;; TODO(stevan): go into error state if response body is of form {"error": ...}
        ;; (if (:error events) true false)

        ;; TODO(stevan): Change executor to return this json object.
        (assert (= (keys events) '(:events))
                (str "execute!: unexpected response body: " events))
        (log/debug :events events)

        (assert (:run-id data) "execute!: no run-id set...")
        (cond (re-matches #"^client:\d+$" (:from body))
          (db/append-history! (:test-id data)
                              (:run-id data)
                              :invoke
                              (:event body)
                              (-> body :args json/write)
                              (-> body :from parse-client-id))
          (= (:kind body) "message")
          (db/append-trace! (:test-id data)
                            (:run-id data)
                            (-> body :event)
                            (-> body :args json/write)
                            (-> body :from)
                            (-> body :to)
                            (-> body :at str)))

        (let [[client-responses internal]
              (partition-haskell #(some? (re-matches #"^client:\d+$" (:to %)))
                                 (:events events))]
          ;; TODO(stevan): use seed to shuffle client-responses?
          (doseq [client-response client-responses]
            (db/append-history! (:test-id data)
                                (:run-id data)
                                :ok ;; TODO(stevan): have SUT decide this?
                                (:event client-response)
                                (-> client-response :args :response json/write)
                                (-> client-response :to parse-client-id)))
          [(assoc data' :clock timestamp) {:events internal}])))))

(comment
  (fake/with-fake-routes
    {"http://localhost:3001/api/v1/event"
     (fn [_request] {:status 200
                     :headers {}
                     :body (json/write {:events
                                        [{:kind "invoke"
                                          :event "inc",
                                          :args {}
                                          :to "client:0"
                                          :from "node1"}]})})}
    (-> (init-data)
        (load-test! {:test-id 1})
        first
        (register-executor {:executor-id "http://localhost:3001/api/v1/event"
                            :components ["node1" "node2"]})
        first
        (create-run! {:test-id 1})
        first
        (execute!)))
  )

(s/def ::timestamped-entries (s/coll-of agenda/entry?))

(>defn timestamp-entries
  [data entries timestamp]
  [::data entries? time/instant?
   => (s/tuple ::data (s/keys :req-un [::timestamped-entries]))]
  (with-bindings {#'gen/*rnd* (java.util.Random. (:seed data))}
    (let [new-seed (.nextLong gen/*rnd*)
          timestamps (->> (gen/vec #(random/exponential 20) (count entries))
                          (mapv #(time/plus-millis timestamp %)))]
      [(assoc data :seed new-seed)
       {:timestamped-entries
        (mapv (fn [entry timestamp]
                (merge entry {:at timestamp})) entries timestamps)}])))

(comment
  (-> (init-data)
      (timestamp-entries
       [{:kind "invoke"
         :event "do-inc"
         :args {}
         :to "inc"
         :from "client"}
        {:kind "invoke"
         :event "do-inc"
         :args {}
         :to "inc"
         :from "client"}]
       (time/instant (time/init-clock)))
      second) )

(>defn enqueue-entry
  [data timestamped-entry]
  [::data agenda/entry? => (s/tuple ::data (s/keys :req-un [::queue-size]))]
  (-> data
      (update :agenda #(agenda/enqueue % timestamped-entry))
      (update :state (fn [state]
                       (case state
                         :responding :responding
                         :error-cannot-enqueue-in-this-state)))
      (ap (fn [data'] {:queue-size (count (:agenda data'))}))))

(comment

  (s/explain-str agenda/entry? {:kind "invoke", :event :a, :args {}, :at (time/instant 0)
                                :from "f", :to "t"})

  (-> (init-data)
      (load-test! {:test-id 1})
      first
      (register-executor {:executor-id "http://localhost:3001" :components ["node1" "node2"]})
      first
      (create-run! {:test-id 1})
      first
      (execute)
      first
      (enqueue-entry {:kind "invoke", :event :a, :args {}, :at (time/instant 0)
                      :from "f", :to "t"})
      )
  )

(>defn enqueue-timestamped-entries
  [data {:keys [timestamped-entries]}]
  [::data (s/keys :req-un [::timestamped-entries])
   => (s/tuple ::data (s/keys :req-un [::queue-size]))]
  (if (and (empty? timestamped-entries) (-> data :agenda empty?))
    [(update data :state
             (fn [state]
               (case state
                 :responding :finished
                 :error-cannot-enqueue-in-this-state)))
     {:queue-size 0}]
    (let [data' (-> (reduce (fn [ih timestamped-entry]
                              (first (enqueue-entry ih timestamped-entry)))
                            data
                            timestamped-entries)
                    (update :state
                            (fn [state] (case state
                                          :responding :requesting
                                          :error-cannot-enqueue-in-this-state))))]
      [data' {:queue-size (count (:agenda data'))}])))

(comment
  (enqueue-timestamped-entries
 (-> (init-data)
     (load-test! {:test-id 1})
     first
     (register-executor {:executor-id "http://localhost:3001" :components ["inc" "store"]})
     first)
 (-> (init-data)
     (timestamp-entries
      [{:kind "invoke"
        :event "do-inc"
        :args {}
        :to "inc"
        :from "client"}
       {:kind "invoke"
        :event "do-inc"
        :args {}
        :to "inc"
        :from "client"}
       ]
      (time/instant (time/init-clock)))
     second) )
)

(>defn step!
  [data]
  [::data => (s/tuple ::data (s/keys :req-un [::queue-size ::entry ::events]))]
  (let [entry (-> data :agenda peek)
        [data' events] (execute! data)
        [data'' timestamped-entries] (timestamp-entries data'
                                                        (:events events)
                                                        (-> data' :clock))
        [data''' queue-size] (enqueue-timestamped-entries data'' timestamped-entries)]
    [data''' (merge {:entry entry} events queue-size)]))

(comment
  (fake/with-fake-routes
    {"http://localhost:3001/api/v1/event"
     (fn [_request] {:status 200
                     :headers {}
                     :body (json/write {:events [{:kind "invoke"
                                                  :event :a
                                                  :args {}
                                                  :to "node1"
                                                  :from "from"}]})})}
    (-> (init-data)
        (load-test! {:test-id 1})
        first
        (register-executor {:executor-id "http://localhost:3001/api/v1/event"
                            :components ["node1"]})
        first
        (create-run! {:test-id 1})
        first
        step!
        first
        step!)) )

(defn run!
  [data]
  (loop [data data steps 0]
    (if (= :finished (:state data))
      [data {:steps steps}]
      (recur (-> data step! first) (inc steps)))))

(comment
  (fake/with-fake-routes
    {"http://localhost:3001/api/v1/event"
     (fn [_request] {:status 200
                     :headers {}
                     :body (json/write {:events []})})}
    (-> (init-data)
        (load-test! {:test-id 1})
        first
        (register-executor {:executor-id "http://localhost:3001/api/v1/event"
                            :components ["node1"]})
        first
        (create-run! {:test-id 1})
        first
        run!))
  )

(>defn status
  [data]
  [::data => (s/tuple ::data ::data)]
  [data data])

(comment
  (status (init-data)) )

(defn set-seed!
  [data {new-seed :new-seed}]
  [(assoc data :seed new-seed) new-seed])

(defn reset
  [_data]
  [(init-data) :reset])

(def fault? (s/keys :req-un [:scheduler.agenda/kind
                             :scheduler.agenda/to
                             :scheduler.agenda/from
                             :scheduler.agenda/at]))

(s/def ::faults (s/coll-of fault?))

(>defn inject-faults!
  [data faults]
  [::data (s/keys :req [::faults]) => (s/tuple ::data map?)]
  [(update data :faults (fn [fs] (apply conj fs (:faults faults))))
   {:ok "added faults"}
   ])
