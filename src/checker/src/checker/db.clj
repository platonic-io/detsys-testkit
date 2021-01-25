(ns checker.db
  (:require
   [checker.json :as json]
   [clojure.pprint :as pp]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.edn :as edn]
   [next.jdbc :as jdbc]
   [next.jdbc.sql :as sql]
   [next.jdbc.result-set :as rs]))

(set! *warn-on-reflection* true)

;; ---------------------------------------------------------------------

(def db nil)
(def ds nil)

(defn setup-db
  [db-file]
  (alter-var-root #'db
                  (constantly {:dbtype "sqlite" :dbname db-file}))
  (alter-var-root #'ds
                  (constantly (jdbc/get-datasource db))))

(defn db
  []
  (or (System/getenv "DETSYS_DB")
      (str (System/getenv "HOME") "/.detsys.db")))

(defn reg-op
  [model op]
  (let [w (case model
            :list-append :append
            :w)]
    (case (:event op)
      "write" [w :x]
      "ack"   [w :x]
      "read"  [:r :x]
      "value" [:r :x])))

(defn op-type
  [op]
  (case (:kind op)
    "invoke" :invoke
    "ok"     :ok
    "fail"   :fail
    "info"   :info))

(defn op-value
  [args]
  (let [v (:value args)]
    (cond
      (int? v) v
      (vector? v) v
      :else nil)))

(defn rewrite-op
  [model op value]
  (-> op
      (dissoc :event :kind :args)
      (assoc :type (op-type op)
             :f :txn
             :value [(conj (reg-op model op) value)])))

(defn rewrite
  "We need to rewrite histories that look like:

      `[:invoke :write, :args {:value 1}, :ok :ack, :args {}]`

  into:

      `[:invoke [[:w :x 1]], :ok [[:w :x 1]]]`

  We use state to keep track of the :args from :invoke for each :process, so that
  we can use the same :args in the response."
  [model [state acc] op]
  (case (op-type op)
    :invoke [(assoc state (:process op) (:args op))
             (conj acc (rewrite-op model op (op-value (:args op))))]
    :ok [state (conj acc (rewrite-op model op (or (op-value (:args op))
                                                  (op-value (get state (:process op))))))]
    :info [state (conj acc (rewrite-op model op (or (op-value (:args op))
                                                    (op-value (get state (:process op))))))]
    :fail (throw "implement later")))

(defn get-history
  [model test-id run-id]
  (->> (jdbc/execute!
        ds
        ["SELECT kind,event,args,process FROM jepsen_history
          WHERE test_id = ? AND run_id = ?"
         test-id run-id]
        {:return-keys true :builder-fn rs/as-unqualified-lower-maps})
       (mapv #(update % :args json/read))
       (map-indexed (fn [ix x] (assoc x :index ix)))
       (reduce (partial rewrite model) [{} []])
       second))

(defn store-result
  [test-id run-id valid? result gitrev]
  (jdbc/execute-one!
   ds
   ["INSERT INTO analysis (test_id, run_id, id, valid, result, checker_version)
     VALUES(?,
            ?,
            (SELECT IFNULL(MAX(id), - 1) + 1 FROM analysis WHERE test_id = ? AND run_id = ?),
            ?,
            ?,
            ?)"
   test-id run-id test-id run-id (if valid? 1 0) (json/write result) gitrev]))

(comment
  (setup-db (db))
  (pp/pprint (get-history :register 2 0) )
  (pp/pprint (get-history :list-append 1 0) )
  (store-result 1 0 1 "{}" "00000") )
