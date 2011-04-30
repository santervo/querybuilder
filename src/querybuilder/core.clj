(ns querybuilder.core
  (:use [clojure.string :only [join] :rename {join str-join}]))

(declare as-str as-str-recursive)

(def all :*)

(defn from [table]
  (let [table (as-str table)]
    {:from table :select {table all}}))

(defn join [query table]
  (update-in query :from #(str % " JOIN " table)))

(defn change-table-selection [query table columns]
  (assoc-in query [:select (as-str table)] (as-str-recursive columns)))

(defn modify-select [query modifications]
  (if (empty? modifications)
    query
    (recur (apply change-table-selection query (first modifications))
           (rest modifications))))

(defn identifier-chain [identifiers]
  (str-join "." identifiers))

(defn as [col-expr new-name]
  (str col-expr " AS " new-name))

(defn table-select-item [table col-name col-new-name]
  (as (identifier-chain [table col-name]) col-new-name))

(defn table-select-items [table col-renames]
  (map (partial apply table-select-item table) col-renames))

(def apply-table-select-items (partial apply table-select-items))

(defn select-items [query]
  (map apply-table-select-items (:select query)))

(defn select-list [query]
  (str-join ", " (select-items query)))

(defn select-expr [query]
  (str "SELECT " (select-list query)))

(defn from-expr [query])

(defn statement [query]
  (str (select-expr query) " " (from-expr query)))

(defn- as-str [x]
  (if (keyword? x)
    (name x)
    (str x)))

(defn as-str-recursive [x]
  (cond
    (map? x) (apply hash-map (flatten (map as-str-recursive x)))
    (or (vector? x) (seq? x)) (map as-str-recursive x)
    :else (as-str x)))


