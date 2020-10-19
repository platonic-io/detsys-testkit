(ns scheduler.checker.model
  (:require [knossos.model :as model])
  (:import (knossos.model Model)))

(defrecord TestModel [val]
  Model
  (step [_this op]
    (case (:f op)
      :do-inc (let [expected (inc val)
                    got (-> op :value :value)]
                (if (= expected got)
                  (->TestModel expected)
                  (model/inconsistent {:expected expected
                                       :got got
                                       :op op}))))))

(defn initial []
  (->TestModel 0))
