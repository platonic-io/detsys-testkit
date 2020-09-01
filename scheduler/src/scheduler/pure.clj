(ns scheduler.pure
  (:require [clojure.spec.alpha :as s]
            [clojure.data.generators :as gen]
            [clj-http.client :as client]
            [scheduler.spec :refer [>defn => component-id?]]
            [scheduler.agenda :as agenda]
            [scheduler.json :as json]
            [taoensso.timbre :as log]))

(s/def ::total-executors     pos-int?)
(s/def ::connected-executors nat-int?)
(s/def ::topology            (s/map-of string? string?))
(s/def ::agenda              agenda/agenda?)
(s/def ::state               #{:started
                               :test-prepared
                               :ready
                               :requesting
                               :responding
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
   :seed                (java.util.Random. 1)
   :total-commands      0
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
      (update :agenda #(agenda/enqueue
                        %
                        ;; TODO(stevan): actually load the test from the db.
                        {:command {:name :a, :parameters []}, :component-id "a"}
                        1))
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

(defn execute
  [data]
  (if-not (contains? #{:ready :responding} (:state data))
    [(assoc data :state :error-cannot-execute-in-this-state) nil]
    (let [[agenda' [entry timestamp]] (agenda/dequeue (:agenda data))
          data' (assoc data :agenda agenda')
          ;; TODO(stevan): handle case when executor-id doesn't exist... Perhaps
          ;; this should be checked when commands are enqueued?
          executor-id (get (:topology data) (:component-id entry))]
      [data' {:url (str executor-id "/api/command")
              :timestamp timestamp
              :body (merge entry {:timestamp timestamp})}])))

(-> (init-data)
    (load-test! {:test-id 1})
    first
    (register-executor {:executor-id "http://localhost:3000" :components ["c"]})
    first
    (execute)
    )

(s/def ::entry agenda/entry?)

(def entries? (s/coll-of (s/keys :req-un [::entry] :kind vector?)))

(def timestamped-entries? (s/coll-of (s/tuple (s/keys :req-un [::entry]) agenda/timestamp?)
                                     :kind seq?))

(>defn timestamp-entries
  [data entries timestamp]
  [::data entries? agenda/timestamp? => (s/tuple ::data timestamped-entries?)]
  (with-bindings {#'gen/*rnd* (:seed data)}
    (let [timestamps (->> (gen/vec #(gen/uniform 1 10000) (count entries))
                          (map #(+ timestamp %)))]
      [(assoc data :seed gen/*rnd*) (map vector entries timestamps)])))


(comment
(timestamp-entries (init-data)
                   [{:entry {:command {:name "do-inc" :parameters []}
                             :component-id "inc"}}
                    {:entry {:command {:name "do-inc" :parameters []}
                             :component-id "inc"}}]
                   1) )

;; TODO(stevan): move to other module?
(>defn execute!
  [data]
  [::data => (s/tuple ::data timestamped-entries?)]
  (let [[data' {:keys [url timestamp body]} :as r] (execute data)]
    (log/debug :execute r)
    ;; TODO(stevan): Retry on failure, this possibly needs changes to executor
    ;; so that we don't end up executing the same command twice.
    (timestamp-entries data' (-> (client/post url {:body (json/write body)})
                                 :body
                                 json/read)
                       timestamp)))

(>defn enqueue-command
  [data {:keys [entry timestamp]}]
  [::data (s/keys :req-un [::entry ::timestamp])
   => (s/tuple ::data (s/keys :req-un [::queue-size]))]
  (-> data
      (update :agenda #(agenda/enqueue % (assoc entry :id (:total-commands data)) timestamp))
      (update :total-commands inc)
      (update :state (fn [state]
                       (case state
                         :responding :responding
                         :error-cannot-enqueue-in-this-state)))
      (ap (fn [data'] {:queue-size (count (:agenda data'))}))))

(defn enqueue-commands
  [data {:keys [commands]}]
  (if (and (empty? commands) (-> data :agenda empty?))
    [(update data :state :finished) {:queue-size 0}]
    (let [data' (reduce (fn [ih command]
                          (first (enqueue-command ih command))) data commands)]
      [data' (count (:agenda data'))])))

(enqueue-commands (-> (init-data)
                      (register-executor {:executor-id "http://localhost:3001" :components ["inc" "store"]})
                      first)
                  {:commands [{:entry {:command {:name "do-inc" :parameters []}
                                       :component-id "inc"}
                               :timestamp 1}]})

(defn status
  [data]
  [data data])

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

(defn set-seed!
  [data {new-seed :new-seed}]
  [(assoc data :seed (java.util.Random. new-seed)) new-seed])


(comment
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
