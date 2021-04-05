(ns ledjer.core
  (:require [clojure.string :as string]
            [clojure.edn :as edn]))

(defn make-transaction [data]
  (select-keys data [:date :description :postings]))

(defn make-posting [data]
  (select-keys data [:account :amount]))

(def x "2021/04/01 Auto voor naar H&F")
(def y "expenses:recreation:covid 20 EUR")

(defn parse-include-line [line]
  (if-let [[_ file-name] (re-matches #"include (\S+)" line)]
    {:include file-name}))

(defn parse-commodity-line [line]
  (if-let [[_ fmt] (re-matches #"commodity (.*)" line)]
    {:format fmt}))

(defn parse-transaction-header [x]
  (if-let [[_ date description]
        (re-matches #"(\d\d\d\d/\d\d/\d\d) (.*)" x)]
    {:date date :description description}))

(defn parse-posting [x]
  (if-let [[_ account amount]
        (re-matches #"\s*([\S:]+)\s+(.*) EUR" x)]
    {:account account :amount (edn/read-string amount)}))

(defn parse-account-name [x]
  (if-let [matches (re-seq #"(\w+)" x)]
    (map #(nth % 1) matches)))

(defn parse-empty-line [x]
  (if (re-matches #"" x)
    true
    false))

(defn parse-line [line]
  ((some-fn parse-include-line parse-commodity-line parse-transaction-header parse-posting) line))

(defn transaction-header? [data]
  (and (:date data) (:description data)))

(defn reduce-fn [acc lines]
  (let [line (first lines)]
    (if (nil? line)
      acc
      (if (transaction-header? line)
        (update acc :transactions conj lines)
        (update acc :header conj lines)))))

(defn read-ledger-file [file-name]
  (->> file-name
       (slurp)
       (string/split-lines)
       (map parse-line)
       (partition-by nil?)
       (reduce reduce-fn {:headers [] :transactions []})))

(defn reduce-fn* [acc line]
  (if-let [header ((some-fn parse-include-line parse-commodity-line) line)]
      (update acc :headers conj header)
      (if-let [transaction-header (parse-transaction-header line)]
        (if-let [current (:current-transaction acc)]
          (-> acc
              (update :transactions conj (:current-transaction acc))
              (assoc :current-transaction transaction-header))
          (assoc acc :current-transaction transaction-header))
        (if-let [posting (parse-posting line)]
          (if (:postings (:current-transaction acc))
            (update-in acc [:current-transaction :postings] conj posting)
            (assoc-in acc [:current-transaction :postings] [posting]))
          acc))))

(reduce reduce-fn* {:headers [] :transactions []}
        ["include beleggingen.journal"
         "include huis.journal"
         "commodity 100.00 EUR"
         ""
         "2020/12/31 Opening balances"
         "    equity:opening-balances  -11.98 EUR"
         "    assets:bank:degiro        11.98 EUR"
         ""
         "2020/12/31 Opening balances"])

(let [file-name "2021.journal"]
  (->> file-name
       (slurp)
       (string/split-lines)
       (reduce reduce-fn* {:headers [] :transactions []})))

(def transactions
  [{:account "expenses:recreation:covid" :amount 30} {:account "expenses:recreation" :amount 20} {:account "income" :amount 10}
   {:account "expenses:recreation" :amount 10}])

(defn accounts
  ([transactions] (accounts transactions {}))
  ([transactions options]
   (->> transactions
        (map :account)
        (distinct)
        (sort))))

(defn map-vals [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn sum-amounts [amounts]
  (apply + amounts))

(defn balancesheet [transactions]
  (->> transactions
       (group-by :account)
       (map-vals #(map :amount %))
       (map-vals sum-amounts)))

(defn display-report [data]
  (print (string/join "\n" data)))

