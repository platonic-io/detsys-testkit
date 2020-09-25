(ns checker.core
  (:require [clojure
             [pprint :refer [pprint]]
             [edn :as edn]]
            [clojure.java.io :as io]
            [elle [core :as elle]
             [list-append :refer :all]])
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

(defn c
  "Check a history."
  [opts history]
  (-> (check opts history)
      ;; We don't care about these; it's kinda redundant.
      (dissoc :also-not)))

(defn cf
  "Checks a file."
  [opts filename]
  (c opts (read-history filename)))

(defn checker
  [filename]
  (cf {:consistency-models [:strict-serializable]} filename))

(defn -main
  [& args]
  (pprint (checker (first args))))
