(ns ledjer.core
  (:require [clojure.algo.generic.functor :refer [fmap]]
            [clojure.edn :as edn]
            [clojure.string :as string]
            [clojure.tools.cli :refer [parse-opts]]
            [environ.core :refer [env]]
            [instaparse.core :as insta]
            [lentes.core :as l]
            [java-time :refer [local-date-time local-date as truncate-to]])
  (:gen-class))

(defn parse-include-line [line]
  (if-let [[_ file-name] (re-matches #"include (\S+)" line)]
    {:include file-name}))

(defn parse-commodity-line [line]
  (if-let [[_ fmt] (re-matches #"commodity (.*)" line)]
    {:commodity fmt}))

(defn parse-budget-header [x]
  (if-let [[_ period] (re-matches #"~(.*)" x)]
    {:budget period}))

(defn parse-transaction-header [x]
  (if-let [[_ date description]
        (re-matches #"(\d\d\d\d/\d\d/\d\d) (.*)" x)]
    {:transaction-header true
     :date (local-date "yyyy/MM/dd" date)
     :description description}))

(defn parse-posting [x]
  (let [[fst scnd] (string/split x #" @@ ")
        posting (if-let [[_ account amount unit]
                         (re-matches #"\s+([\S:]+)\s+(-?\d+\.?\d*) (\S+)" fst)]
                  {:posting true
                   :account account
                   :amount {(keyword unit) (bigdec (edn/read-string amount))}})]
    (if scnd
      (if-let [[_ amount unit] (re-matches #"(-?\d+\.?\d*) (\S+)" scnd)]
        (assoc posting :purchase-price {(keyword unit) (bigdec (edn/read-string amount))})
        posting)
      posting)))

(defn parse-empty-line [x]
  (if (re-matches #"" x)
    {:empty-line true}))

(defn parse-price-line [x]
  (if-let [[_ date commodity price]
           (re-matches #"P (\d\d\d\d/\d\d/\d\d) (\S+) (.*) EUR" x)]
    {:date (local-date "yyyy/MM/dd" date)
     :commodity commodity
     :price (bigdec (edn/read-string price))}))

(defn tokenize [contents]
  (map (some-fn parse-include-line parse-commodity-line parse-budget-header parse-transaction-header parse-posting parse-empty-line parse-price-line) contents))

(defn fsm [tokens]
  "Finite state machine to parse the tokens of a ledger file into the internal representation. Consists of the following functions:

  parse-general: parse singular tokens or delegate parsing of the next group of tokens in case of a header

  parse-budget: parse a budget headers and the corresponding postings

  parse-transaction: parse a single transaction"
  (letfn
    [(parse-general [acc [t & ts :as all]]
       #(if t
          (cond
            (:include t)
            (parse-general (update acc :headers conj t) ts)
            (:price t)
            (parse-general (update-in acc
                                      [:prices (keyword (:commodity t))]
                                      conj
                                      (select-keys t [:date :price]))
                           ts)
            (:commodity t)
            (parse-general (update acc :headers conj t) ts)
            (:empty-line t)
            (parse-general acc ts)
            (:budget t)
            (parse-budget acc all)
            (:transaction-header t)
            (parse-transaction acc all)
            :else
            nil)
          acc))

     (parse-budget [acc tokens]
       #(loop [acc acc
               b-acc {}
               [t & ts :as all] tokens]
          (cond
            (:budget t)
            (recur acc (assoc (assoc b-acc :period (:budget t))
                                     :postings []) ts)
            (:account t)
            (recur acc (update b-acc :postings conj (dissoc t :posting)) ts)
            :else
            (parse-general (update acc :budgets conj b-acc) all))))

     (parse-transaction [acc tokens]
       #(loop [acc acc
               t-acc {}
               [t & ts :as all] tokens]
          (if t
            (cond
              (:transaction-header t)
              (recur acc (assoc (dissoc t :transaction-header)
                                :postings []) ts)
              (:account t)
              (recur acc
                     (update t-acc :postings conj
                             (dissoc t :posting))
                     ts)
              :else
              (parse-general (update acc :transactions conj t-acc) all))
          (update acc :transactions conj t-acc))))]

    (trampoline parse-general {:headers []
                               :prices {}
                               :transactions []} tokens)))

(defn read-ledger-file [contents]
  (->> contents
       (string/split-lines)
       (tokenize)
       (fsm)))

(defn accounts [journal]
  (->> (:transactions journal)
      (mapcat :postings)
      (map :account)
      (distinct)
      (sort)))

(defn amount->str [amount]
  (string/join ", " (map (fn [[k v]] (str v " " (name k))) amount)))

(defn balancesheet [transactions]
  (->> transactions
       (fmap (partial map :amount))
       (fmap #(apply merge-with + %))
       (fmap amount->str)))

(defn make-posting [account amount]
  {:account account :amount amount})

(defn make-transaction [date description postings]
  {:description description
   :date date
   :postings (map (partial apply make-posting) postings)})

(defn monthly [transactions]
  (->> transactions
       (group-by #(as (:date %) :year :month-of-year))))

(defn account-view [transactions]
  (->> transactions
       (mapcat (fn [{date :date
                     description :description
                     postings :postings}]
                 (for [p postings]
                   (let [{account :account} p]
                     {account [(assoc p :date date :description description)]}))))
       (apply merge-with into)))

(defn lpad [length s]
  (format (str "%" length "s") s))

(defn rpad [length s]
  (format (str "%-" length "s") s))

(defn make-report
  "Build a report of the transactions. Output format is a map with [account period] as key and the aggregated value as value."
  ([transactions] (make-report balancesheet transactions))
  ([make-report-fn transactions]
   (->> transactions
        (account-view)
        (fmap monthly)
        (fmap make-report-fn)
        (mapcat (fn [[account values]]
                  (for [[header data] values]
                    {[account header] data})))
        (apply merge-with into))))

(defn transpose [m]
  (apply mapv vector m))

(defn matrixmap [f m]
  (mapv (partial mapv f) m))

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

(def cli-options
  [["-f" "--file NAME" "File name to use" ]
   ["-h" "--help"]])

(defn error-msg [errors]
  (str "The following errors occurred while parsing the arguments:"
       (string/join \newline errors)))

(defn usage [options-summary]
  (->> ["This is ledjer. Ledjer is a cli-bookkeeping program based on programs like ledger and hledger,"
        "implemented in Clojure."
        "It's more a learning project that meant for regular use."
        ""
        "Options:"
        options-summary
        ""
        "Actions:"
        "  balancesheet    Display a balancesheet"
        ""
        "Please refer to the source code for more information ;-)"]
       (string/join \newline)))

(defn validate-args [args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options) {:ok? true :exit-message "help!"}
      errors {:ok? false :exit-message "error"}
      (and (= 1 (count arguments))
           (#{"accounts" "balancesheet" "bs"} (first arguments)))
      {:action (first arguments) :options options}
      :else {:ok? false :exit-message (usage options)})))

(defn accounts! [journal options]
  (let [report (accounts journal)]
    (println (string/join "\n" report))))

(defn balancesheet! [journal options]
  (let [report (->> (:transactions journal)
                    (make-report balancesheet))
        rheaders (sort (distinct (map first (keys report))))
        cheaders (sort (distinct (map second (keys report))))
        data (for [rh rheaders]
               (for [ch cheaders]
                 (get report [rh ch])))]
    (println (table->string rheaders cheaders data))))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn -main [& args]
  (let [{:keys [action options exit-message ok?]} (validate-args args)]
    (if exit-message
      (exit (if ok? 1 0) exit-message)
      (let [file-name (or (:file options)
                          (env :ledjer-file))
            journal (-> file-name
                        (slurp)
                        (read-ledger-file))]
        (cond
          (#{"accounts"} action) (accounts! journal options)
          (#{"balancesheet" "bs"} action) (balancesheet! journal options))))))

(def parser (insta/parser
              "S = (INCLUDE-LINE|COMMODITY-LINE|<EMPTY-LINE>|TRANSACTION)*
              INCLUDE-LINE = <'include '> WORD <EOL>
              COMMODITY-LINE = <'commodity '> NUMBER <WS> WORD <EOL>
              EMPTY-LINE = <EOL>
              TRANSACTION = TRANSACTION-HEADER POSTING+
              TRANSACTION-HEADER = DATE <WS> DESCRIPTION <EOL>
              POSTING = <WS> ACCOUNT (<WS> AMOUNT PURCHASE_PRICE?)? <EOL>
              AMOUNT = NUMBER <WS> WORD
              PURCHASE_PRICE = <' @@ '> NUMBER <WS> WORD
              DATE = #'\\d\\d\\d\\d/\\d\\d/\\d\\d'
              DESCRIPTION = #'[^\r\n]*'
              ACCOUNT = #'[\\S:]+'

              WS = #'\\s+'
              NUMBER = #'-?\\d+\\.?\\d*'
              WORD = #'\\S*'
              EOL = '\\r'?'\\n'"))

(defn read-word [node]
  (if (= :WORD (first node))
    (second node)))

(defn read-number [node]
  (if (= :NUMBER (first node))
    (bigdec (edn/read-string (second node)))))

(defn read-date [node]
  (let [[id date] node]
    (if (= :DATE id)
      (local-date "yyyy/MM/dd" date))))

(defn read-description [node]
  (let [[id desc] node]
    (if (= :DESCRIPTION id)
      desc)))

(defn read-account [node]
  (let [[id value] node]
    (if (= :ACCOUNT id)
      value)))

(defn read-purchase-price [node]
  (let [[id number word] node]
    (if (= :PURCHASE-PRICE id)
      {(keyword (read-word word)) (read-number number)})))

(defn read-amount [node]
  (let [[id number word] node]
    (if (= :AMOUNT id)
      {(keyword (read-word word)) (read-number number)})))

(defn read-posting [node]
  (let [[id account amount purchase-price] node]
    (if (= :POSTING id)
      (let [p {:account (read-account account)
               :amount (read-amount amount)}]
        (if purchase-price
          (assoc p :purchase-price (read-purchase-price purchase-price))
          p)))))

(defn read-transaction-header [node]
  (let [[id date description] node]
    (if (= :TRANSACTION-HEADER id)
      {:date (read-date date)
       :description (read-description description)})))

(defn read-transaction [node]
  (let [[id header & postings] node]
    (if (= :TRANSACTION id)
      (let [t (read-transaction-header header)]
        (assoc t :postings (map read-posting postings))))))

(defn read-include [node]
  (let [[id word] node]
    (if (= :INCLUDE-LINE id)
      {:include (read-word word)})))

(defn read-commodity [node]
  (let [[id number word] node]
    (if (= :COMMODITY-LINE id)
      {:commodity {(keyword (read-word word)) (read-number number)}})))

(defn read-start [node]
  (let [[id & lines] node]
    (if (= :S id)
      (map (some-fn read-include read-transaction read-commodity) lines))))

(comment "TODO: add tests, add price, add budget")
(read-start
  (parser (string/join "\n" ["include some.file"
                             "include someother.file"
                             "commodity 100.00 EUR"
                             ""
                             "2021/01/01 buy apples"
                             "  expenses:groceries 1 EUR"
                             "  assets:checking   -1 EUR"
                             ""
                             "2021/01/01 buy apples"
                             "  assets:apples     2 APPLE @@ 1 EUR"
                             "  assets:checking"
                             ""])))
