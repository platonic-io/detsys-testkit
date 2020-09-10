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
            [taoensso.timbre :as log]))

(set! *warn-on-reflection* true)

(s/def ::total-executors     pos-int?)
(s/def ::connected-executors nat-int?)
(s/def ::topology            (s/map-of string? string?))
(s/def ::agenda              agenda/agenda?)
(s/def ::state               #{:started
                               :test-prepared
                               :ready
                               :requesting
                               :responding
                               :finished
                               :error-cannot-load-test-in-this-state
                               :error-cannot-register-in-this-state
                               :error-cannot-enqueue-in-this-state
                               :error-cannot-execute-in-this-state})

(s/def ::data (s/keys :req-un [::total-executors
                               ::connected-executors
                               ::topology
                               ::agenda
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
                     :test-prepared state'
                     :waiting-for-executors state'
                     :error-cannot-register-in-this-state))))
      (ap (fn [data']
            {:remaining-executors (max 0 (- (:total-executors data')
                                            (:connected-executors data')))}))))

(defn create-run!
  [data {:keys [test-id]}]
  )

(defn execute
  [data]
  (if-not (contains? #{:ready :requesting} (:state data))
    [(assoc data :state :error-cannot-execute-in-this-state) nil]
    (let [[agenda' entry] (agenda/dequeue (:agenda data))
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
      (assert executor-id (str "Target `" (:to entry) "' isn't in topology."))
      [data' {:url (str executor-id "/api/command")
              :timestamp (:at entry)
              :body entry}])))
(comment
  (-> (init-data)
      (load-test! {:test-id 1})
      first
      (register-executor {:executor-id "http://localhost:3000" :components ["component0"]})
      first
      (execute)) )

(def entry? (s/keys :req-un [::command
                             ::to
                             ::from]))

(def entries? (s/coll-of entry? :kind vector?))

(s/def ::responses entries?)

(defn error-state?
  [state]
  (some? (re-matches #"^error-.*$" (name state))))

;; TODO(stevan): move to other module, since isn't pure?
;; TODO(stevan): can we avoid using nilable here? Return `Error + Data *
;; Response` instead of always `Data * Response`? Or `Data * Error + Data * Response`?
(>defn execute!
  [data]
  [::data => (s/tuple ::data (s/nilable (s/keys :req-un [::responses])))]
  ;; TODO(stevan): timestamp not used?
  (let [[data' {:keys [url _timestamp body]}] (execute data)]
    (if (error-state? (:state data'))
      [data' nil]
      ;; TODO(stevan): Retry on failure, this possibly needs changes to executor
      ;; so that we don't end up executing the same command twice.
      (let [responses (-> (client/post url {:body (json/write body)})
                          :body
                          json/read)]
        ;; TODO(stevan): Change executor to return this json object.
        (assert (= (keys responses) '(:responses))
                (str "execute!: unexpected response body: " responses))

        ;; TODO(stevan): append `body` (the popped entry) to history here!

        [data' responses]))))

(comment
  (fake/with-fake-routes
    {"http://localhost:3001/api/command"
     (fn [_request] {:status 200
                     :headers {}
                     :body (json/write {:responses
                                        [{:command {:name "b" :parameters []}
                                          :to "component0"
                                          :from "clinet0"}]})})}
    (-> (init-data)
        (load-test! {:test-id 1})
        first
        (register-executor {:executor-id "http://localhost:3001" :components ["component0"]})
        first
        (execute!)))
  )

(s/def ::timestamped-entries (s/coll-of agenda/entry?))

(>defn timestamp-entries
  [data entries timestamp]
  [::data entries? agenda/timestamp?
   => (s/tuple ::data (s/keys :req-un [::timestamped-entries]))]
  (with-bindings {#'gen/*rnd* (java.util.Random. (:seed data))}
    ;; TODO(stevan): define and use exponential distribution instead.
    (let [new-seed (.nextLong gen/*rnd*)
          timestamps (->> (gen/vec #(gen/uniform 1 10000) (count entries))
                          (mapv #(+ timestamp %)))]
      [(assoc data :seed new-seed)
       {:timestamped-entries
        (mapv (fn [entry timestamp]
                (merge entry {:at timestamp})) entries timestamps)}])))

(comment
  (-> (init-data)
      (timestamp-entries
       [{:command {:name "do-inc" :parameters []}
         :to "inc"
         :from "client"}
        {:command {:name "do-inc" :parameters []}
         :to "inc"
         :from "client"}]
       1)
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
        [{:command {:name "do-inc" :parameters []}
          :to "inc"
          :from "client"}
         {:command {:name "do-inc" :parameters []}
          :to "inc"
          :from "client"}]
        1)
       second)) )

(>defn step!
  [data]
  [::data => (s/tuple ::data (s/keys :req-un [::queue-size ::entry ::responses]))]
  (let [entry (-> data :agenda peek)
        [data' responses] (execute! data)
        [data'' timestamped-entries] (timestamp-entries data' (:responses responses) 1)
        [data''' queue-size] (enqueue-timestamped-entries data'' timestamped-entries)]
    [data''' (merge {:entry entry} responses queue-size)]))

(comment
  (fake/with-fake-routes
    {"http://localhost:3001/api/command"
     (fn [_request] {:status 200
                     :headers {}
                     :body (json/write {:responses []
                                      ;; [{:entry {:command {:name "b" :parameters []}
                                      ;;           :to "c"}}]
                                        })})}
    (-> (init-data)
        (load-test! {:test-id 1})
        first
        (register-executor {:executor-id "http://localhost:3001" :components ["component0"]})
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
    {"http://localhost:3001/api/command"
     (fn [_request] {:status 200
                     :headers {}
                     :body (json/write {:responses []
                                      ;; [{:entry {:command {:name "b" :parameters []}
                                      ;;           :to "c"}}]
                                        })})}
    (-> (init-data)
        (load-test! {:test-id 1})
        first
        (register-executor {:executor-id "http://localhost:3001" :components ["component0"]})
        first
        run!))
  )

(>defn status
  [data]
  [::data => (s/tuple ::data ::data)]
  [data data])

(comment
  (status (init-data)))

(defn set-seed!
  [data {new-seed :new-seed}]
  [(assoc data :seed new-seed) new-seed])
