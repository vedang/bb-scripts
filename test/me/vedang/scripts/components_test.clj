(ns me.vedang.scripts.components-test
  (:require [me.vedang.scripts.components :as sut]
            [clojure.test :as t]))

(t/deftest clj-files
  (t/is (= (set ["src/bb-scripts/components.clj"
                 "src/bb-scripts/util.clj"])
           (set (sut/clj-files ["src/bb-scripts/components.clj"
                                "src/bb-scripts/namespaces.edn"
                                "test/bb-scripts/test.clj"
                                "qa/bb-scripts/runner.clj"
                                "qa/bb-scripts/runner.sh"
                                "src/bb-scripts/util.clj"]
                               ["test/" "qa/"])))))
