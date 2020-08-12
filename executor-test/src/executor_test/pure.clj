(ns executor-test.pure)

(defn init-data
  []
  {:store 0})


(defn component-inc
  [data cmd timestamp]
  (case (:name cmd)
    "do-inc" {:data data
              :output [{:component-id "store"
                        :command {:name "fetch"
                                  :parameters {:return "inc"
                                               :return-name "$set-value"}}}]}
    "$set-value" {:data data
                  :output [{:component-id "store"
                            :command {:name "store"
                                      :parameters {:value (-> cmd :parameters :value inc)}}}]}))

(defn component-store
  [data cmd _timestamp]
  (case (:name cmd)
    "fetch" {:data data
             :output [{:component-id (-> cmd :parameters :return)
                       :command {:parameters {:value (:store data)}
                                 :name (-> cmd :parameters :return-name)}}]}
    "store" {:data {:store (-> cmd :parameters :value)}
             :output []}
    "reset" {:data {:store 0}
             :output []}))

(defn command-executor
  [data body]
  (case (:component-id body)
    "inc" (component-inc data (:command body) (:timestamp body))
    "store" (component-store data (:command body) (:timestamp body))
    (do
      (println (str "Unknown component " (:component-id body) " don't know what to do!"))
      {:data data
       :output []})))
