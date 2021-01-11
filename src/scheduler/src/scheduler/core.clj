(ns scheduler.core
  (:require [ring.middleware.defaults :as rmd]
            [ring.middleware.reload :as rmr]
            [ring.adapter.jetty :as jetty]
            [scheduler.handler :as handler]
            [scheduler.db :as db]
            [scheduler.pure :as pure])
  (:import [org.eclipse.jetty.server
            Server]
           [lockfix LockFix])
  (:gen-class))

(set! *warn-on-reflection* true)

;; patched version of clojure.core/locking to workaround GraalVM unbalanced
;; monitor issue. Compile lockfix with:
;; javac java/src/lockfix/LockFix.java -cp \
;;       ~/.m2/repository/org/clojure/clojure/1.10.2-alpha4/clojure-1.10.2-alpha4.jar
(defmacro locking*
  "Executes exprs in an implicit do, while holding the monitor of x.
  Will release the monitor of x in all circumstances."
  {:added "1.0"}
  [x & body]
  `(let [lockee# ~x]
     (LockFix/lock lockee# (^{:once true} fn* [] ~@body))))

(defn instrument
  ([] (instrument (#'clojure.spec.test.alpha/instrumentable-syms)))
  ([sym-or-syms] (instrument sym-or-syms nil))
  ([sym-or-syms opts]
   (locking* #'clojure.spec.test.alpha/instrumented-vars
             (into
              []
              (comp (filter (#'clojure.spec.test.alpha/instrumentable-syms opts))
                    (distinct)
                    (map #(#'clojure.spec.test.alpha/instrument-1 % opts))
                    (remove nil?))
              (#'clojure.spec.test.alpha/collectionize sym-or-syms)))))

(defn unstrument
  ([] (unstrument (map #'clojure.spec.test.alpha/->sym
                       (keys @#'clojure.spec.test.alpha/instrumented-vars))))
  ([sym-or-syms]
   (locking* #'clojure.spec.test.alpha/instrumented-vars
             (into
              []
              (comp (filter symbol?)
                    (map #'clojure.spec.test.alpha/unstrument-1)
                    (remove nil?))
              (#'clojure.spec.test.alpha/collectionize sym-or-syms)))))

(alter-var-root #'clojure.spec.test.alpha/instrument (constantly instrument))
(alter-var-root #'clojure.spec.test.alpha/unstrument (constantly unstrument))

(defonce server (atom nil))

(defn- start-server
  ([]
   (start-server false 3000))
  ([reload port]
   (db/setup-db (or (System/getenv "DETSYS_DB")
                    (str (System/getenv "HOME") "/.detsys.db")))
   (reset! handler/data (pure/init-data))
   (reset! server (jetty/run-jetty (cond-> #'handler/app
                                     true (rmd/wrap-defaults rmd/api-defaults)
                                     ;; NOTE: wrap-reload needs to be disabled
                                     ;; when native-image is compiled and run.
                                     reload rmr/wrap-reload)
                                   {:port port
                                    :join? false}))))

(defn- stop-server
  []
  (when @server (.stop ^Server @server))
  (reset! server nil))

(defn- restart-server
  []
  (stop-server)
  (start-server))

(defn -main
  [& args]
  (let [port (Integer/parseInt (or (System/getenv "DETSYS_SCHEDULER_PORT") "3000"))
        arg (first args)]
    (when (or (= arg "--version")
              (= arg "-v"))
      (do (println pure/gitrev)
          (System/exit 0)))
    (start-server false port)
    (println (str "Running webserver at http:/127.0.0.1:" port "/"))))

(comment
  (start-server true 3000)
  (stop-server)
  (restart-server) )
