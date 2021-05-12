(ns ledjer.core-test
  (:require [clojure.test :refer :all]
            [ledjer.core :refer :all]))

(deftest core
  (testing "validate-args"
    (is (= {:action "balancesheet" :options {}} (validate-args ["balancesheet"])))
    (is (= {:action "balancesheet" :options {:file "filename"}}
           (validate-args ["balancesheet" "-f" "filename"])))
    (is (= {:action "accounts" :options {}} (validate-args ["accounts"])))
    (is (= false (:ok? (validate-args ["non-existing"]))))))

(def data
  [["2021/01/01" "Buy an apple"
    [["expenses:groceries" 0.45M]
     ["assets:checking" -0.45M]]]
   ["2021/01/15" "Buy a lemon"
    [["expenses:lemons" 0.30M]
     ["assets:checking" -0.30M]]]
   ["2021/02/01" "Buy another apple"
    [["expenses:groceries" 0.45M]
     ["assets:checking" -0.45M]]]
   ["2021/02/03" "Buy a red grapefruit"
    [["expenses:grapegruits" 0.80M]
     ["assets:checking" -0.80M]]]])
