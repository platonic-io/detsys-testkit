(ns scheduler.spec
  (:require [clojure.spec.alpha :as s]
            [ghostwheel.core :as g]))

(def component-id? string?)

(defmacro >defn
  {:style/indent 1
   :style.cljfmt/indent [[:block 2] [:inner 1]]}
  [& args]
  `(g/>defn ~@args))

(def => g/=>)
