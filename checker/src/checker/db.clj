(ns checker.db
  (:require
   [clojure.java.shell :as shell]
   [checker.json :as json]
   [clojure.pprint :as pp]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.edn :as edn]))

(set! *warn-on-reflection* true)

;; ---------------------------------------------------------------------

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
      (dissoc :event :test_id :run_id :kind :args)
      (set/rename-keys {:id :index})
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

(def db (or (System/getenv "DETSYS_DB")
            (str (System/getenv "HOME") "/.detsys.db")))

(defn query [& args]
  (apply shell/sh "sqlite3" db args))

(defn parse
  [test-id run-id id kind event args process]
  {:test-id (edn/read-string test-id)
   :run-id  (edn/read-string run-id)
   :id      (edn/read-string id)
   :kind    kind
   :event   event
   :args    (json/read args)
   :process (edn/read-string process)})

(defn get-history
  [model test-id run-id]
  (let [out (query (str "SELECT * FROM history where test_id = "
                        test-id " AND run_id = " run-id))]
    (when (not= 0 (:exit out))
      (println out))
    (try
      (->> out
           :out
           str/split-lines
           (map #(str/split % #"\|"))
           (map (partial apply parse))
           (reduce (partial rewrite model) [{} []])
           second)
      (catch Exception e
        (println out)
        (println model)
        (println test-id)
        (println run-id)
        (println e)
        (System/exit 1)))))

(comment
  (pp/pprint (get-history :rw-register 6 0)))
