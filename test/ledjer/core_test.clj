(ns ledjer.core-test
  (:require [clojure.test :refer :all]
            [ledjer.core :refer :all]
            [java-time :refer [local-date]]))

(deftest core
  (testing "validate-args"
    (is (= {:action "balancesheet" :options {}} (validate-args ["balancesheet"])))
    (is (= {:action "balancesheet" :options {:file "filename"}}
           (validate-args ["balancesheet" "-f" "filename"])))
    (is (= {:action "accounts" :options {}} (validate-args ["accounts"])))
    (is (= false (:ok? (validate-args ["non-existing"])))))

  (testing "parse-include-line"
    (is (= {:include "filename.ledger"} (parse-include-line "include filename.ledger"))))

  (testing "parse-commodity-line"
    (is (= {:commodity "100.00 EUR"} (parse-commodity-line "commodity 100.00 EUR")))
    (is (= {:commodity "100,00 EUR"} (parse-commodity-line "commodity 100,00 EUR")))
    (is (= {:commodity "$100.00"} (parse-commodity-line "commodity $100.00"))))

  (testing "parse-budget-header"
    (is (= {:budget "monthly"} (parse-budget-header "~monthly"))))

  (testing "parse-transaction-header"
    (is (= {:transaction-header true
            :date (local-date "yyyy/MM/dd" "2021/01/02")
            :description "Shopping"}
           (parse-transaction-header "2021/01/02 Shopping"))))

  (testing "parse-price-line"
    (is (= {:price 12.34M
            :commodity "VEV"
            :date (local-date "yyyy/MM/dd" "2021/02/03")}
           (parse-price-line "P 2021/02/03 VEV 12.34 EUR"))))

  (testing "parse-posting"
    (is (= {:posting true :account "expenses:shoes" :amount {:EUR 100.0M}}
           (parse-posting "   expenses:shoes    100.00 EUR"))
        (= {:posting true :account "expenses:shoes" :amount {:STOCK 100.0M :purchase-price {:EUR 1}}}
           (parse-posting "   assets:stocks    100.00 STOCK @@ 1 EUR")) ))

  (testing "parse-empty-line"
    (is (= {:empty-line true} (parse-empty-line ""))
        (= nil (parse-empty-line "abcd"))))

  (testing "tokenize"
    (is (= (list {:include "somefile.extension"}
                 {:include "some-other-file.extension"}
                 {:empty-line true}
                 {:commodity "100.00 EUR"}
                 {:budget "monthly"}
                 {:posting true :account "expenses:groceries" :amount {:EUR 300.0M}}
                 {:posting true :account "expenses:games" :amount {:COIN 20.0M}}
                 {:empty-line true}
                 {:transaction-header true
                  :date (local-date "yyyy/MM/dd" "2021/01/01")
                  :description "apples"}
                 {:posting true :account "expenses:groceries" :amount {:EUR 5.0M}}
                 {:posting true :account "assets:checking" :amount {:EUR -5.0M}})
           (tokenize ["include somefile.extension"
                      "include some-other-file.extension"
                      ""
                      "commodity 100.00 EUR"
                      "~monthly"
                      "   expenses:groceries 300.00 EUR"
                      "   expenses:games      20.00 COIN"
                      ""
                      "2021/01/01 apples"
                      "   expenses:groceries   5.00 EUR"
                      "   assets:checking     -5.00 EUR"]))))

  (testing "fsm"
    (is (= {:headers [{:include "something"}
                      {:include "something"}
                      {:commodity "100.00 EUR"}]
            :transactions [{:date (local-date "yyyy/MM/dd" "2021/01/01")
                            :description "Buy apples"
                            :postings [{:account "expenses:groceries"
                                        :amount {:EUR 5.0M}}
                                       {:account "assets:checking"
                                        :amount {:EUR -5.0M}}]}
                           {:date (local-date "yyyy/MM/dd" "2021/01/02")
                            :description "Buy more apples"
                            :postings [{:account "expenses:groceries"
                                        :amount {:COIN 7.5M}}
                                       {:account "assets:checking"
                                        :amount {:COIN -7.5M}}]}]
            :budgets [{:period "monthly"
                      :postings [{:account "expenses:groceries"
                                  :amount {:EUR 150.0M}}
                                 {:account "expenses:apples"
                                  :amount {:EUR 50.0M}}]}]
            :prices {:VEV (list {:date (local-date "yyyy/MM/dd" "2021/02/03")
                                 :price 12.34M})}}
           (fsm (tokenize ["include something"
                           "include something"
                           "commodity 100.00 EUR"
                           ""
                           "P 2021/02/03 VEV 12.34 EUR"
                           ""
                           "~monthly"
                           "  expenses:groceries   150.00 EUR"
                           "  expenses:apples       50.00 EUR"
                           ""
                           "2021/01/01 Buy apples"
                           "  expenses:groceries  5.00 EUR"
                           "  assets:checking    -5.00 EUR"
                           ""
                           "2021/01/02 Buy more apples"
                           "   expenses:groceries 7.50 COIN"
                           "   assets:checking   -7.50 COIN"
                           ])))))

  (testing "table->string"
    (let [rheaders ["aap" "noot" "mies"]
          cheaders ["bananen" "citroenen" "limoenen"]
          data [[1 2 3] [4 5 6] [7 8 9]]]
      (is (= "     | bananen | citroenen | limoenen\naap  | 1 | 2 | 3\nnoot | 4 | 5 | 6\nmies | 7 | 8 | 9" (table->string rheaders cheaders data))))))


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
