(ns checker.db
  (:require
   [checker.json :as json]
   [clojure.pprint :as pp]
   [clojure.set :as set]
   [mount.core :as mount]
   [next.jdbc :as jdbc]
   [next.jdbc.sql :as sql]
   [next.jdbc.result-set :as rs]))

(set! *warn-on-reflection* true)

;; ---------------------------------------------------------------------

;; The following stuff related to db connections comes from:
;; https://grishaev.me/en/clj-sqlite/

(def spec
  {:dbname    "../db/detsys.sqlite3"
   :classname "org.sqlite.JDBC"
   :dbtype    "sqlite"})

(declare db)

(defn on-start
  []
  (let [conn (jdbc/get-connection spec)]
    (assoc spec :connection conn)))

(defn on-stop
  []
  (let [^java.sql.Connection conn (:connection db)]
    (-> conn .close))
  nil)

(mount/defstate
  ^{:on-reload :noop}
  db
  :start (on-start)
  :stop (on-stop))

(mount/start #'db)

;; ---------------------------------------------------------------------

(defn reg-op
  [op]
  (case (:event op)
    "write" [:w :x]
    "ack"   [:w :x]
    "read"  [:r :x]
    "value" [:r :x]))

(defn op-type
  [op]
  (case (:kind op)
    "invoke" :invoke
    "ok"     :ok
    "fail"   :fail
    "info"   :info))

(defn op-value
  [args]
  (if (int? (:value args))
    (:value args)
    nil))

(defn rewrite-op
  [op value]
  (-> op
      (dissoc :event :test_id :run_id :kind :args)
      (set/rename-keys {:id :index})
      (assoc :type (op-type op)
             :f :txn
             :value [(conj (reg-op op) value)])))

(defn rewrite
  "We need to rewrite histories that look like:

      `[:invoke :write, :args {:value 1}, :ok :ack, :args {}]`

  into:

      `[:invoke [[:w :x 1]], :ok [[:w :x 1]]]`

  We use state to keep track of the :args from :invoke for each :process, so that
  we can use the same :args in the response."
  [[state acc] op]
  (case (op-type op)
    :invoke [(assoc state (:process op) (:args op))
             (conj acc (rewrite-op op (op-value (:args op))))]
    :ok [state (conj acc (rewrite-op op (or (op-value (:args op))
                                            (op-value (get state (:process op))))))]
    :fail (throw "implement later")
    :info (throw "implement later")))

(defn get-history
  [test-id run-id]
  (->> (sql/find-by-keys db :history {:test_id test-id, :run_id run-id}
                         {:builder-fn rs/as-unqualified-lower-maps})
       (mapv #(update % :args json/read))
       (reduce rewrite [{} []])
       second))

(comment
  (pp/pprint (get-history 1 0)) )
