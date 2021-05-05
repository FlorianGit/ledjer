(ns ledjer.core
  (:require [clojure.algo.generic.functor :refer [fmap]]
            [clojure.string :as string]
            [ledjer.parser :as parser]
            [lentes.core :as l]
            [java-time :refer [local-date-time local-date as truncate-to]]))

(defn accounts [journal]
  (->> (:transactions journal)
      (mapcat :postings)
      (map :account)
      (distinct)
      (sort)))

(defn balancesheet [transactions]
  (->> transactions
       (fmap (partial map :amount))
       (fmap (partial apply +))))


(let [transactions 
      (-> "2021.journal"
          (slurp)
          (parser/read-ledger-file)
          (:transactions))]
  (->> transactions
       (balancesheet)))

(defn make-posting [account amount]
  {:account account :amount amount})

(defn make-transaction [date description postings]
  {:description description
   :date date
   :postings (map (partial apply make-posting) postings)})

(defn c* [fns]
  "Cartesian product of functions"
  (fn [values]
    (mapv (fn [f x] (f x)) fns values)))

(defn cmap [fns coll]
  "Map the cartesian product of fns over coll"
  (mapv (c* fns) coll))

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


(defn monthly [transactions]
  (->> transactions
       (group-by #(as (:date %) :year :month-of-year))))

(def transactions
  (-> "2021.journal"
      (slurp)
      (parser/read-ledger-file)
      (:transactions)))

(defn account-view [transactions]
  (->> transactions
       (mapcat (fn [{date :date
                     description :description
                     postings :postings}]
                 (for [p postings]
                   (let [{ account :account} p]
                     {account [(assoc p :date date :description description)]}))))
       (apply merge-with into)))

(defn lpad [length s]
  (format (str "%" length "s") s))

(defn rpad [length s]
  (format (str "%-" length "s") s))

(defn report [transactions]
  "Build a report of the transactions"
  (->> transactions
       (account-view)
       (fmap monthly)
       (fmap balancesheet)
       (mapcat (fn [[account values]]
                 (for [[header data] values]
                   {[account header] data})))
       (apply merge-with into)))

(let [report (report transactions)]
  (let [accounts (distinct (map first (keys report)))
        accounts-length (apply max (map count accounts))
        columns (distinct (map second (keys report)))
        column-lengths (into {} (for [c columns] [c (->> accounts
                                             (mapv #(get report [% c]))
                                             (map #(count (str %)))
                                             (apply max))]))]
    (for [a (sort accounts)]
      (str (rpad accounts-length a)
      (apply str (for [c columns]
        (lpad (inc (get column-lengths c)) (if-let [amount (get report [a c])]
                                             (str amount)
                                             ""))))))))

(defn transpose [m]
  (apply mapv vector m))

(defn matrixmap [f m]
  (mapv (partial mapv f) m))

(comment
  (defn table->string [data]
    "Converts a vector of vectors (a vector of rows) to a string representation"
    (let [columns (transpose data)
          widths (->> columns
                      (matrixmap str)
                      (matrixmap count)
                      (mapv (partial apply max)))]
      (string/join \newline (for [row data]
                              (string/join " | " (mapv rpad widths row))))))
  
  )

(defn table->string [rheaders cheaders data]
  (let [first-width (->> rheaders
                         (mapv count)
                         (apply max))
        padded-rheaders (->> rheaders
                             (mapv (partial rpad first-width)))
        str-data (matrixmap str data)
        widths (->> str-data
                    (transpose)
                    (matrixmap count)
                    (mapv (partial apply max)))
        padded-data (->> str-data
                         (mapv (fn [row]
                                 (mapv lpad widths row))))
        first-row (into [(lpad first-width "")] (mapv rpad widths cheaders))
        rows (mapv (fn [rh row]
                     (into [rh] row))
                   padded-rheaders padded-data)
        ]
    (string/join "\n" (mapv (partial string/join " | ") (into [first-row] rows)))))
