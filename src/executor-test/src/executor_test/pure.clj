(ns executor-test.pure)

(defn init-data
  []
  {:store 0
   :inc-messages 0
   :inc-sessions {}})

(defn inc-message
  [data]
  (update data :inc-messages inc))

(defn store-client
  [data from]
  (update data :inc-sessions (fn [sessions] (assoc sessions (:inc-messages data) from))))

(defn component-inc
  [data from cmd timestamp]
  (case (:name cmd)
    "do-inc" {:data (-> data (store-client from) inc-message)
              :output [{:component-id "store"
                        :command {:name "fetch"
                                  :parameters {:return "inc"
                                               :session-id (:inc-messages data)
                                               :return-name "$set-value"}}}]}
    "$set-value" {:data (inc-message data)
                  :output [{:component-id "store"
                            :command {:name "store"
                                      :parameters {:value (-> cmd :parameters :value inc)}}}
                           {:component-id (-> data :inc-sessions (get (-> cmd :parameters :session-id)))
                            :command {:name "do-inc"
                                      :parameters {:value (-> cmd :parameters :value inc)}}}]}))

(defn component-store
  [data _from cmd _timestamp]
  (case (:name cmd)
    "fetch" {:data data
             :output [{:component-id (-> cmd :parameters :return)
                       :command {:parameters {:value (:store data)
                                              :session-id (-> cmd :parameters :session-id)}
                                 :name (-> cmd :parameters :return-name)}}]}
    "store" {:data {:store (-> cmd :parameters :value)}
             :output []}
    "reset" {:data (assoc data :store 0)
             :output []}))

(defn command-executor
  [data body]
  (let [from (:from body)]
    (case (:component-id body)
      "inc" (component-inc data from (:command body) (:timestamp body))
      "store" (component-store data from (:command body) (:timestamp body))
      (do
        (println (str "Unknown component " (:component-id body) " don't know what to do!"))
        {:data data
         :output []}))))
