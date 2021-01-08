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
(s/def ::seed                integer?)
(s/def ::agenda              agenda/agenda?)
(s/def ::clock               time/instant?)
(s/def ::next-tick           time/instant?)
(s/def ::tick-frequency      double?)
(s/def ::min-time-ns         double?)
(s/def ::max-time-ns         double?)
(s/def ::client-requests     (s/coll-of agenda/entry?))
(s/def ::client-timeout-ms   double?)
(s/def ::client-delay-ms     double?)
(s/def ::logical-clock       nat-int?)
(s/def ::state               #{:started
                               :test-prepared
                               :inits-prepared
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
                               ::seed
                               ::agenda
                               ::faults
                               ::clock
                               ::next-tick
                               ::tick-frequency
                               ::min-time-ns
                               ::max-time-ns
                               ::client-requests
                               ::client-timeout-ms
                               ::client-delay-ms
                               ::logical-clock
                               ::state]))

(s/def ::executor-id string?)
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
   :next-tick           (time/init-clock)
   :tick-frequency      50.0
   :min-time-ns         0.0
   :max-time-ns         0.0
   :client-requests     []
   :client-timeout-ms   (* 30.0 1000)
   :client-delay-ms     (* 1.0 1000)
   :logical-clock       0
   :state               :started})

(defn ap
  [data f]
  [data (f data)])

(s/def ::test-id nat-int?)
(s/def ::queue-size nat-int?)

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

(defn create-run!
  [data {:keys [test-id]}]
  (case (:state data)
    :inits-prepared
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
      (register-executor! {:executor-id "http://localhost:3000" :components ["component0"]})
      first
      (create-run! {:test-id 1})))

(defn should-drop?
  [data entry]
  (let [faults (:faults data)
        entry' (-> entry
                   (select-keys [:to :from])
                   (assoc :kind "omission"
                          :at (:logical-clock data)))]
    ((set faults) entry')))

(defn from-client?
  [body]
  (-> body :from (re-matches #"^client:\d+$")))

(s/def ::url string?)
(s/def ::timestamp time/instant?)
(s/def ::body agenda/entry?)
(s/def ::drop? #{:keep :drop :delay})

(>defn fetch-new-entry!
  [data]
  [::data => (s/tuple ::data (s/nilable
                              (s/keys :req-un [::url
                                               ::timestamp
                                               ::body
                                               ::drop?])))]
  (if-not (contains? #{:ready :requesting} (:state data))
    [(assoc data :state :error-cannot-execute-in-this-state) nil]
    (let [[agenda' entry] (agenda/dequeue (:agenda data))
          entry-from-client-with-current-request (some #(= (-> % :from)
                                                           (-> entry :from))
                                                       (:client-requests data))
          data' (-> data
                    (assoc :agenda agenda'
                           :clock (:at entry))
                    (assoc :agenda (if entry-from-client-with-current-request
                                     (agenda/enqueue agenda' (update entry :at #(time/plus-millis % (:client-delay-ms data))))
                                     agenda'))
                    (update :logical-clock (if entry-from-client-with-current-request identity inc))
                    (update :state (fn [state]
                                     (case state
                                       :ready :responding
                                       :requesting :responding
                                       :error-cannot-execute-in-this-state))))
          ;; TODO(stevan): handle case when executor-id doesn't exist... Perhaps
          ;; this should be checked when commands are enqueued?
          executor-id (get (:topology data') (:to entry))]
      (assert executor-id (str "Target `" (:to entry) "' isn't in topology."))
      [data' {:url executor-id
              :timestamp (:at entry)
              :body entry
              :drop? (cond
                       (should-drop? data' entry) :drop
                       entry-from-client-with-current-request :delay
                       :else :keep)}])))

(comment
  (-> (init-data)
      (update
       :agenda
       #(agenda/enqueue
         %
         {:kind "invoke", :event :a, :args {}, :at (time/instant 0), :from "f", :to "t"}))
      :agenda
      agenda/dequeue))

(comment
  (-> (init-data)
      (load-test! {:test-id 1})
      first
      (register-executor! {:executor-id "http://localhost:3000/api/v1/event"
                          :components ["node1" "node2"]})
      first
      (create-run! {:test-id 1})
      first
      (fetch-new-entry!)))

(s/def ::kind string?)
(s/def ::event string?)
(s/def ::args map?)
(s/def ::to string?)
(s/def ::from string?)

(def entry? (s/and (s/keys :req-un [::kind
                                    ::event
                                    ::args
                                    ::to
                                    ::from])
                   #(not (#{"timer"} (:kind %)))))

(def entries? (s/coll-of entry? :kind vector?))

(def timer?
  (s/and (s/keys :req-un [::kind
                          ::args
                          ::from
                          ::duration-ns])
         #(= (:kind %) "timer")))

(def event?
  (s/or :entry entry?
        :timer timer?))

(def events? (s/coll-of event? :kind vector?))

(s/def ::events events?)

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
  (partition-haskell odd? []))

(>defn parse-client-id
  [s]
  [string? => nat-int?]
  (->> s (re-matches #"^client:(\d+)$") second Integer/parseInt))

(comment
  (parse-client-id "client:0"))

(>defn add-client-request
  [data body]
  [::data agenda/entry? => ::data]
  (update data :client-requests (fn [m] (conj m body))))

(>defn remove-client-requests
  [data clients]
  [::data (s/coll-of string?) => ::data]
  (let [clients-set (set clients)]
    (update data :client-requests (fn [m]
                                    (remove (fn [body]
                                              (contains? clients-set (:from body))) m)))))

(>defn expired-clients
  [data current-time]
  [::data time/instant? => (s/tuple ::data (s/coll-of agenda/entry?))]
  (let [expired? (fn [body] (time/before? (time/plus-millis (:at body)
                                                           (:client-timeout-ms data))
                                         current-time))
        [to-drop to-keep] (partition-haskell expired? (-> data :client-requests))]
    [(assoc data :client-requests to-keep)
     to-drop]))

;; TODO(stevan): move to other module, since isn't pure?
;; TODO(stevan): can we avoid using nilable here? Return `Error + Data *
;; Response` instead of always `Data * Response`? Or `Data * Error + Data * Response`?
(>defn execute!
  [data]
  [::data => (s/tuple ::data (s/nilable (s/keys :req-un [::events])))]
  (let [[data' {:keys [url timestamp body drop?]}] (fetch-new-entry! data)]
    (cond
      (error-state? (:state data')) [data nil]
      (= drop? :delay) (do
                         (log/debug :delaying-message body)
                         [data' {:events []}])
      :else (let [[data' expired-clients] (expired-clients data' timestamp)
                  _ (doseq [client expired-clients]
                      (db/append-history! (:test-id data')
                                          (:run-id data')
                                          :info
                                          (:event client)
                                          (-> client :args json/write)
                                          (-> client :from parse-client-id)))
                  is-from-client? (re-matches #"^client:\d+$" (:from body))
                  dropped? (= drop? :drop)
                  sent-logical-time (or (-> body :sent-logical-time)
                                        (and is-from-client?
                                             (:logical-clock data)))]
              (db/append-trace! (:test-id data)
                                (:run-id data)
                                (-> body :event)
                                (-> body :args json/write)
                                (-> body :kind)
                                (-> body :from)
                                (-> body :to)
                                sent-logical-time
                                (-> data' :logical-clock)
                                dropped?)
              (db/append-time-mapping! (:test-id data)
                                       (:run-id data)
                                       (-> data' :logical-clock)
                                       (-> data' :clock))
              (if dropped?
                (do
                  (log/debug :dropped? dropped? :clock (:clock data'))
                  [data' {:events []}])
                (let ;; TODO(stevan): Retry on failure, this possibly needs changes to executor
                    ;; so that we don't end up executing the same command twice.
                    [_ (db/append-event! (:test-id data) (:run-id data) "Send message to executor" "Start")
                     events (-> (client/post (str url (if (= (:kind body) "timer") "timer" "event"))
                                             {:body (json/write body) :content-type "application/json; charset=utf-8"})
                                :body
                                json/read)
                     _ (db/append-event! (:test-id data) (:run-id data) "Send message to executor" "End")]
                  ;; TODO(stevan): go into error state if response body is of form {"error": ...}
                  ;; (if (:error events) true false)

                  ;; TODO(stevan): Change executor to return this json object.
                  (assert (= (keys events) '(:events))
                          (str "execute!: unexpected response body: " events))
                  (log/debug :events events)

                  (assert (:run-id data) "execute!: no run-id set...")
                  (when is-from-client?
                    (db/append-history! (:test-id data)
                                        (:run-id data)
                                        :invoke
                                        (:event body)
                                        (-> body :args json/write)
                                        (-> body :from parse-client-id)))

                  (let [[client-responses internal]
                        (partition-haskell #(and (#{"ok"} (:kind %))
                                                 (some? (re-matches #"^client:\d+$" (:to %))))
                                           (:events events))
                        internal (mapv #(assoc % :sent-logical-time (:logical-clock data')) internal)
                        data'' (cond-> data'
                                 is-from-client? (add-client-request body)
                                 (not (empty? client-responses)) (update :logical-clock inc)
                                 true (remove-client-requests (map :to client-responses)))]
                    ;; TODO(stevan): use seed to shuffle client-responses?
                    (if (not (empty? client-responses))
                      (db/append-time-mapping! (:test-id data'')
                                               (:run-id data'')
                                               (-> data'' :logical-clock)
                                               (-> data'' :clock)))
                    (doseq [client-response client-responses]
                      (db/append-history! (:test-id data)
                                          (:run-id data)
                                          :ok ;; TODO(stevan): have SUT decide this?
                                          (:event client-response)
                                          (-> client-response :args :response json/write)
                                          (-> client-response :to parse-client-id))
                      (db/append-trace! (:test-id data)
                                        (:run-id data)
                                        (-> client-response :event)
                                        (-> client-response :args json/write)
                                        "ok"
                                        (-> client-response :from)
                                        (-> client-response :to)
                                        (-> data' :logical-clock) ;; should we advance clock for client responses?
                                        (-> data'' :logical-clock)
                                        false))
                    [data'' {:events internal}])))))))
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
        (register-executor! {:executor-id "http://localhost:3001/api/v1/event"
                            :components ["node1" "node2"]})
        first
        (create-run! {:test-id 1})
        first
        (execute!))))

(s/def ::timestamped-entries (s/coll-of agenda/entry?))

(>defn timestamp-entries
  [data entries timestamp]
  [::data events? time/instant?
   => (s/tuple ::data (s/keys :req-un [::timestamped-entries]))]
  (with-bindings {#'gen/*rnd* (java.util.Random. (:seed data))}
    (let [new-seed (.nextLong gen/*rnd*)
          timestamps (->> (gen/vec #(random/exponential 20) (count entries))
                          (mapv #(time/plus-millis timestamp %)))
          update-entry (fn [entry timestamp]
                         (case (:kind entry)
                           "timer" (-> entry
                                       ;; Maybe these should have another probability distribution for how slow they are
                                       (assoc :at (time/plus-nanos timestamp (double (:duration-ns entry)))
                                              :to (:from entry)
                                              :event :timer)
                                       (dissoc :duration))
                           (assoc entry :at timestamp)))]
      [(assoc data :seed new-seed)
       {:timestamped-entries (mapv update-entry entries timestamps)}])))

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
       (time/init-clock))
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
      (register-executor! {:executor-id "http://localhost:3001" :components ["node1" "node2"]})
      first
      (create-run! {:test-id 1})
      first
      (fetch-new-entry!)
      first
      (enqueue-entry {:kind "invoke", :event :a, :args {}, :at (time/instant 0)
                      :from "f", :to "t"})))

(defn min-time?
  [data]
  (time/before? (time/plus-nanos (time/init-clock)
                                 (:min-time-ns data))
                (:clock data)))

(comment
  (min-time? {:clock (time/init-clock), :min-time-ns 0.0})
  ;; => false
  (min-time? {:clock (time/init-clock), :min-time-ns 1.0})
  ;; => false
  (min-time? {:clock (time/plus-nanos (time/init-clock) 1.0), :min-time-ns 1.0})
  ;; => false
  (min-time? {:clock (time/plus-nanos (time/init-clock) 2.0), :min-time-ns 1.0})
  ;; => true
  )

(defn max-time?
  [data]
  (and (not= (:max-time-ns data) 0.0)
       (time/before? (time/plus-nanos (time/init-clock)
                                      (:max-time-ns data))
                     (:clock data))))

(comment
  (max-time? {:clock (time/init-clock), :max-time-ns 0.0})
  ;; => false
  (max-time? {:clock (time/init-clock), :max-time-ns 1.0})
  ;; => false
  (max-time? {:clock (time/plus-nanos (time/init-clock) 1.0), :max-time-ns 1.0})
  ;; => false
  (max-time? {:clock (time/plus-nanos (time/init-clock) 2.0), :max-time-ns 1.0})
  ;; => true
  )

(>defn enqueue-timestamped-entries
  [data {:keys [timestamped-entries]}]
  [::data (s/keys :req-un [::timestamped-entries])
   => (s/tuple ::data (s/keys :req-un [::queue-size]))]
  (if (or (and (empty? timestamped-entries) (-> data :agenda empty?) (min-time? data))
          (max-time? data))
    [(update data :state
             (fn [state]
               (case state
                 :responding :finished
                 :executors-prepared :inits-prepared
                 :error-cannot-enqueue-in-this-state)))
     {:queue-size (-> data :agenda count)}]
    (let [data' (-> (reduce (fn [ih timestamped-entry]
                              (first (enqueue-entry ih timestamped-entry)))
                            data
                            timestamped-entries)
                    (update :state
                            (fn [state] (case state
                                          :responding :requesting
                                          :executors-prepared :inits-prepared
                                          :error-cannot-enqueue-in-this-state))))]
      [data' {:queue-size (count (:agenda data'))}])))

(comment
  (enqueue-timestamped-entries
   (-> (init-data)
       (load-test! {:test-id 1})
       first
       (register-executor! {:executor-id "http://localhost:3001" :components ["inc" "store"]})
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
          :from "client"}]
        (time/instant (time/init-clock))
        0)
       second)))

(>defn get-initial-events!
  "Get all the initial events for each component that run on an executor."
  [data {:keys [executor-id]}]
  [::data (s/keys :req-un [::executor-id])
   => (s/tuple ::data (s/keys :req-un [::queue-size]))]
  (let [events (-> (client/get (str executor-id "/inits"))
                   :body
                   json/read
                   :events)
        _ (log/debug :get-initial-events executor-id events (:state data))
        [data' timestamped-entries] (timestamp-entries data events (:clock data))
        [data'' queue-size] (enqueue-timestamped-entries data' timestamped-entries)]
    (log/debug :get-initial-events executor-id (count events))
    [data'' queue-size]))

(>defn register-executor!
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
      (get-initial-events! {:executor-id executor-id})
      first
      (ap (fn [data']
            {:remaining-executors (max 0 (- (:total-executors data')
                                            (:connected-executors data')))}))))

(comment
  (let [executor-id "http://localhost:3001"
        components ["components0"]]
    (apply hash-map
           (interleave components
                       (replicate (count components)
                                  executor-id)))))


(>defn tick!
 [data]
 [::data => (s/tuple ::data (s/nilable (s/keys :req-un [::events])))]
 (assert (or (not (empty? (:agenda data)))
             (not (min-time? data))))
 (let [all-events (transient [])]
   (doseq [[component url] (:topology data)]
     (let [url (str url "tick")
           events (-> (client/put url
                                  {:body (json/write {:at (:next-tick data)
                                                      :component component})
                                   :content-type "application/json; charset=utf-8"})
                      :body
                      json/read)]
       (assert (= (keys events) '(:events))
               (str "execute!: unexpected response body: " events))
       (doseq [event (:events events)]
         (conj! all-events event))))
   (let [events (->> all-events
                     persistent!
                     (mapv #(assoc % :sent-logical-time (-> data :logical-clock inc))))]
     [(-> data
          (update :next-tick (fn [c] (time/plus-millis c (:tick-frequency data))))
          (update :logical-clock (if (empty? events) identity inc))
          (assoc :state :responding) ;; probably??
          (assoc :clock (:next-tick data)))
      {:events events}])))

(>defn execute-or-tick!
  [data]
  [::data => (s/tuple ::data (s/nilable (s/keys :req-un [::events])))]
  (if-let [entry (-> data :agenda peek)]
    (if (time/before? (:next-tick data) (:at entry))
      (tick! data)
      (execute! data))
    (if (time/before? (:next-tick data) (time/plus-nanos (time/init-clock)
                                                         (:min-time-ns data)))
      (tick! data)
      (do
        (Thread/sleep 10000)
        [data {:events []}]))))

(>defn step!
  [data]
  [::data => (s/tuple ::data (s/keys :req-un [::events ::queue-size]))]
  (db/append-event! (:test-id data) (:run-id data) "step!" "Start")
  (let [[data' events] (execute-or-tick! data)
        [data'' timestamped-entries] (timestamp-entries data'
                                                        (:events events)
                                                        (-> data' :clock))
        [data''' queue-size] (enqueue-timestamped-entries data'' timestamped-entries)]
    (db/append-event! (:test-id data) (:run-id data) "step!" "End")
    [data''' (merge events queue-size)]))

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
        (register-executor! {:executor-id "http://localhost:3001/api/v1/event"
                            :components ["node1"]})
        first
        (create-run! {:test-id 1})
        first
        step!
        first
        step!)))

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
        (register-executor! {:executor-id "http://localhost:3001/api/v1/event"
                            :components ["node1"]})
        first
        (create-run! {:test-id 1})
        first
        run!)))

(>defn status
  [data]
  [::data => (s/tuple ::data ::data)]
  [data data])

(comment
  (status (init-data)))


(s/def ::new-seed integer?)

(>defn set-seed!
  [data {new-seed :new-seed}]
  [::data (s/keys :req-un [::new-seed]) => (s/tuple ::data integer?)]
  [(assoc data :seed new-seed) new-seed])

(s/def ::new-tick-frequency (s/or :integer integer? :double double?))

(>defn set-tick-frequency!
  [data {new-tick-frequency :new-tick-frequency}]
  [::data (s/keys :req-un [::new-tick-frequency]) => (s/tuple ::data double?)]
  (let [tick-frequency (double new-tick-frequency)]
    [(assoc data :tick-frequency tick-frequency) tick-frequency]))

(s/def ::new-min-time-ns (s/or :integer integer? :double double?))
(s/def ::new-max-time-ns (s/or :integer integer? :double double?))

(>defn set-min-time!
  [data {new-min-time-ns :new-min-time-ns}]
  [::data (s/keys :req-un [::new-min-time-ns]) => (s/tuple ::data double?)]
  (let [min-time (double new-min-time-ns)]
    [(assoc data :min-time-ns min-time) min-time]))

(>defn set-max-time!
  [data {new-max-time-ns :new-max-time-ns}]
  [::data (s/keys :req-un [::new-max-time-ns]) => (s/tuple ::data double?)]
  (let [max-time (double new-max-time-ns)]
    [(assoc data :max-time-ns max-time) max-time]))

(defn reset
  [_data]
  [(init-data) :reset])

(s/def ::at nat-int?)

(def fault? (s/keys :req-un [:scheduler.agenda/kind
                             :scheduler.agenda/to
                             :scheduler.agenda/from
                             ::at]))

(s/def ::faults (s/coll-of fault?))

(>defn inject-faults!
  [data faults]
  [::data (s/keys :req-un [::faults]) => (s/tuple ::data map?)]
  [(update data :faults (fn [fs] (apply conj fs (:faults faults))))
   {:ok "added faults"}])

;; Since the version is a constant GraalVM will evaluate it at compile-time, and
;; it will stay fixed independent of run-time values of the environment
;; variable.
(def gitrev ^String
  (or (System/getenv "DETSYS_SCHEDULER_VERSION")
      "unknown"))

(s/def ::gitrev string?)

(>defn version
  [data]
  [::data => (s/tuple ::data (s/keys :req-un [::gitrev]))]
  [data {:gitrev gitrev}])
