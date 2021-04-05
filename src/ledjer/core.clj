(ns ledjer.core
  (:require [clojure.string :as string]
            [clojure.edn :as edn]))

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

(defn add-transaction [journal t]
  (update journal :transactions conj t))

(defn add-posting [journal p]
  "Add posting p to last transaction of journal. Throws exception if there is no last transaction."
  (let [transactions (:transactions journal)
        last-transaction (peek transactions)
        new-transaction (update last-transaction :postings conj p)]
        (assoc journal :transactions (conj (pop transactions) new-transaction))))

(defn reduce-fn [acc line]
  (if-let [header ((some-fn parse-include-line parse-commodity-line) line)]
    (update acc :headers conj header)
    (if-let [transaction-header (parse-transaction-header line)]
      (add-transaction acc transaction-header)
      (if-let [posting (parse-posting line)]
        (add-posting acc posting)
        acc))))

(defn read-ledger-file [file-name]
  (->> file-name
       (slurp)
       (string/split-lines)
       (reduce reduce-fn {:headers [] :transactions []})))

(defn accounts [journal]
  (->> (:transactions journal)
      (mapcat :postings)
      (map :account)
      (distinct)
      (sort)))

(defn map-vals [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn sum-amounts [amounts]
  (apply + amounts))

(defn balancesheet [journal]
  (->> (:transactions journal)
       (mapcat :postings)
       (group-by :account)
       (map-vals #(map :amount %))
       (map-vals sum-amounts)))

(defn display-report [data]
  (print (string/join "\n" data)))

