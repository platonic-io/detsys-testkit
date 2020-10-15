(ns checker.core
  (:require [clojure
             [pprint :refer [pprint]]
             [edn :as edn]]
            [clojure.java.io :as io]
            [elle [core :as elle]
             [list-append :as list-append]
             [rw-register :as rw-register]])
  (:import (java.io PushbackReader)
           [lockfix LockFix])
  (:gen-class))

(set! *warn-on-reflection* true)

(defmacro locking* ;; patched version of clojure.core/locking to workaround
                   ;; GraalVM unbalanced monitor issue.
  "Executes exprs in an implicit do, while holding the monitor of x.
  Will release the monitor of x in all circumstances."
  {:added "1.0"}
  [x & body]
  `(let [lockee# ~x]
     (LockFix/lock lockee# (^{:once true} fn* [] ~@body))))

(let [orig (atom nil)
      monitor (Object.)]
  (defn log-capture!
    ([logger-ns]
     (log-capture! logger-ns :info :error))
    ([logger-ns out-level err-level]
     (locking* monitor
               (compare-and-set! orig nil [System/out System/err])
               (System/setOut  (#'clojure.tools.logging/log-stream out-level logger-ns))
               (System/setErr (#'clojure.tools.logging/log-stream err-level logger-ns)))))
  (defn log-uncapture!
    []
    (locking* monitor
              (when-let [[out err :as v] @orig]
                (swap! orig (constantly nil))
                (System/setOut out)
                (System/setErr err)))))

(alter-var-root #'clojure.tools.logging/log-capture! (constantly log-capture!))
(alter-var-root #'clojure.tools.logging/log-uncapture! (constantly log-uncapture!))

(defn read-history
  "Reads a history of op maps from a file."
  [filename]
  (with-open [r (PushbackReader. (io/reader filename))]
    (->> (repeatedly #(edn/read {:eof nil} r))
         (take-while identity)
         vec)))

(defn checker-rw-register
  [filename]
  (-> (rw-register/check
       {:consistency-models [:strict-serializable]
        :linearizable-keys? true}
       (read-history filename))
      (dissoc :also-not)))

(defn checker-list-append
  [filename]
  (-> (list-append/check
       {:consistency-models [:strict-serializable]}
       (read-history filename))
      (dissoc :also-not)))

(checker-rw-register "./history/rw-register.edn")

(defn -main
  [& args]
  (case (first args)
    "rw-register" (pprint (checker-rw-register (second args)))
    "list-append" (pprint (checker-list-append (second args)))
    (println
     "First argument should be a model, i.e. either \"rw-register\" or \"list-append\"")))
