(ns scheduler.checker
  (:require
   [knossos.wgl :as wgl]
   [scheduler.checker.model :as model]))

(defn entry-to-knossos-entry
  [entry]
  (let [[type process] (if (= :client (-> entry :from :type))
                         [:invoke (-> entry :from :name)]
                         [:ok (-> entry :to :name)])]
    {:type type
     :process process
     :f (-> entry :command :name keyword)
     :value (-> entry :command :parameters)
     :timestamp (-> entry :at)}))

(defn agenda-to-knossos-history
  [history]
  (let [any-client? (fn [entry]
                      (or (= (-> entry :from :type) :client)
                          (= (-> entry :to :type) :client)))]
    (->> history
         (filter any-client?)
         (map entry-to-knossos-entry))))

(defn check-by-model
  [history]
  (let [knossos-history (agenda-to-knossos-history history)
        model (model/initial)]
    (wgl/analysis model knossos-history)))


(def executor-test-agenda
  (let [entry (fn [command from to at]
                {:command command :from from :to to :at at})
        client0 {:name "client0"
                 :type :client}
        client1 {:name "client1"
                 :type :client}
        inc-comp {:name "inc"
                  :type :component}
        store-comp {:name "store"
                    :type "component"}
        inc-com  {:name "do-inc"
                  :parameters []}
        fetch-com {:name "fetch"
                   :parameters {:return "inc"
                                :return-name "$set-value"}}
        set-value (fn [val] {:name "$set-value"
                            :parameters {:value val}})
        store (fn [val] {:name "store"
                        :parameters {:value val}})
        ret-inc (fn [val] {:name "do-inc"
                          :parameters {:value val}})
        ]
    [(entry inc-com       client0    inc-comp   0)
     (entry inc-com       client1    inc-comp   1)
     (entry fetch-com     inc-comp   store-comp 2)
     (entry fetch-com     inc-comp   store-comp 3)
     (entry (set-value 0) store-comp inc-comp   4)
     (entry (set-value 0) store-comp inc-comp   5)
     (entry (store 1)     inc-comp   store-comp 6)
     (entry (ret-inc 1)   inc-comp   client0    7)
     (entry (store 1)     inc-comp   store-comp 8)
     (entry (ret-inc 1)   inc-comp   client1    9)]))
