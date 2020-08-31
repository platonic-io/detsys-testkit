(ns scheduler.handler-test
  (:require [scheduler.handler :as sut]
            [scheduler.pure :as pure]
            [scheduler.json :as json]
            [clojure.test :as t]))

(defn fixture
  [f]
  (reset! sut/data (pure/init-data))
  (f))

(t/use-fixtures :once fixture)

(def executor1 "http://executor1")

(t/deftest route-test
  (t/is (= {:status 200
            :headers {"Content-Type" "application/json; charset=utf-8"}
            :body (json/write (pure/init-data))}
           (sut/app {:request-method :post
                     :uri "/"
                     :body (json/write
                            {:command "status"
                             :parameters {}})})))
  (t/is (= {:status 200
            :headers {"Content-Type" "application/json; charset=utf-8"}
            :body (json/write {:remaining-executors 0})}
           (sut/app {:request-method :post
                     :uri "/"
                     :body (json/write
                            {:command "register-executor"
                             :parameters
                             {:executor-id executor1
                              :components ["component1" "component2"]}})})))
  (t/is (= {:status 200
            :headers {"Content-Type" "application/json; charset=utf-8"}
            :body (json/write {:queue-size 1})}
           (sut/app {:request-method :post
                     :uri "/"
                     :body (json/write
                            {:command "enqueue-command"
                             :parameters
                             {:entry {:command {:name "command1"
                                                :parameters []}
                                      :component-id "component1"}
                              :timestamp 1}})})))

  (t/is (= {:status 200
            :headers {"Content-Type" "application/json; charset=utf-8"}
            :body (json/write {:total-executors 1
                               :connected-executors 1
                               :topology {"component2" executor1
                                          "component1" executor1}
                               :agenda {{:command {:name "command1"
                                                   :parameters []}
                                         :component-id "component1"}
                                        1}
                               :state :running}) ;; NOTE: Try `SPC m e e` just
                                                 ;; before this comment, the
                                                 ;; JSON seralisation of agenda
                                                 ;; doesn't seem to encode
                                                 ;; commands...
            }
           (sut/app {:request-method :post
                     :uri "/"
                     :body (json/write
                            {:command "status"
                             :parameters {}})})))
  (t/is (= {:status 200
            :headers {"Content-Type" "application/json; charset=utf-8"}
            :body (json/write {:url (str executor1 "/api/command")
                               :body {:command {:name "command1"
                                                :parameters []}
                                      :component-id "component1"
                                      :timestamp 1}})}
           (sut/app {:request-method :post
                     :uri "/"
                     :body (json/write
                            {:command "execute"
                             :parameters {}})}))))
