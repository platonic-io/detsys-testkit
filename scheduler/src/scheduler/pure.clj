(ns scheduler.pure)

(defn init-data
  []
  {:total-executors     1
   :connected-executors 0
   :components          {}
   :state               :started
   })

(defn state-transition
  [data]
  (update data :state
          (fn [state]
            (case state
              :started (if (= (:connected-executors data) (:total-executors data))
                         :ready
                         :waiting-for-executors)
              state))))

(defn ap
  [data transition output]
  {:data   (transition data)
   :output (output data)})

(defn register-executor
  [data {:keys [executor-id components]}]
  (-> data
      (update :connected-executors inc)
      (update :components #(merge % (apply hash-map
                                           (interleave components
                                                       (replicate (count components)
                                                                  executor-id)))))
      (ap state-transition
            #(- (:total-executors %) (:connected-executors %)))))

;; (register-executor (init-data) {:executor-id "exec", :components ["a" "b"]})
