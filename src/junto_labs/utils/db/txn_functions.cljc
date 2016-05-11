(ns junto-labs.utils.db.txn-functions
           (:require [junto-labs.utils.core :as u
                       :refer [->ex fn-not nnil? nempty? ffilter
                               #?@(:clj [fn->> fn-> <- if-cljs])]]
                     [datascript.core       :as mdb]
             #?(:clj [datomic.api           :as db ])
             #?(:clj [junto-labs.utils.db   :as dbu
                       :refer [defn!]              ]))
  #?(:cljs (:require-macros 
                     [junto-labs.utils.core
                       :refer [fn->> fn-> <-]])))

#?(:clj
(defn define-std-db-fns!
  "Transacts all the enclosed 'standard' database functions into the global db connection."
  []
  (defn! q  [[datomic.api :as api]] [db query] (api/q query db))
  (defn! fq [[datomic.api :as api]] [db query] (ffirst (api/q query db)))
  (defn! first  [] [db coll] (first  coll))
  (defn! ffirst [] [db coll] (ffirst coll))
 
  ; MACROS

  (defn! throw [] [db expr] (throw (Exception. expr)))

  (defn! when
    []
    ^{:macro? true}
    [db pred then]
    (when (api/invoke db :fn/eval db pred)
          (api/invoke db :fn/eval db then)))

  (defn! if
    [[datomic.api :as api]]
    ^{:macro? true}
    [db pred then else]
    (if (api/invoke db :fn/eval db pred)
        (api/invoke db :fn/eval db then)
        (api/invoke db :fn/eval db else)))

  (defn! apply-or
    [[datomic.api :as api]]
    ^{:macro? true
      :doc "Variadic |or|."}
    [db args]
    (loop [args-n args]
      (if-let [arg (->> args-n first (api/invoke db :fn/eval db))]
        arg
        (recur (rest args-n)))))

  (defn! or
    [[datomic.api :as api]]
    ^{:macro? true
      :doc "2-arity |or|."}
    [db alt0 alt1]
    (or (api/invoke db :fn/eval db alt0)
        (api/invoke db :fn/eval db alt1)))

  (defn! nil?  [] [db expr] (nil? expr))
  (defn! nnil? [] [db expr] (not (nil? expr)))

  (defn! lookup
    [[datomic.api :as api]]
    [db a v]
    (ffirst
      (api/q [:find '?e :where ['?e a v]] db)))

  (defn! validate
    ^{:macro? true
      :doc    "|eval|s @expr. If the result satisifies @pred, returns @expr.
               Otherwise, throws an assertion error."}
    [[datomic.api :as api]]
    [db pred expr]
    (let [expr-eval (api/invoke db :fn/eval db expr)]
      (if (api/invoke db pred db expr-eval)
          expr-eval
          (throw (ex-info (str "Assertion not met: " pred)
                   {:pred pred :expr expr :expr-eval expr-eval})))))

  (defn! lookup-nnil
    [[datomic.api :as api]]
    [db a v]
    (api/invoke db :fn/eval db
      `(:fn/validate :fn/nnil?
         (:fn/lookup ~a ~v))))

  (defn! eval
    [[clojure.walk :as walk]]
    [db expr]
    (let [db-eval #(datomic.api/invoke db :fn/eval db %)]
      #_(println "EXPR IS" expr
        "CALLABLE?" (and (instance? java.util.List expr) ; is callable?
                 (-> expr first keyword?)
                 (-> expr first namespace (= "fn"))))
      (cond (and (instance? java.util.List expr) ; is callable?
                 (-> expr first keyword?)
                 (-> expr first namespace (= "fn")))
            (let [[f & args] expr
                  macros #{:fn/if :fn/when :fn/validate :fn/or :fn/apply-or :fn/lookup-nnil}]
              (if (contains? macros f)
                  (apply datomic.api/invoke db f db args)
                  (apply datomic.api/invoke db f db
                    (mapv db-eval args)))) ; args are evaluated in order unless is a macro
            
            (instance? clojure.lang.IMapEntry expr) (vec (map db-eval expr))
            (seq? expr) (doall (map db-eval expr))
            (instance? clojure.lang.IRecord expr)
              (reduce (fn [r x] (conj r (db-eval x))) expr expr)
            (coll? expr) (into (empty expr) (map db-eval expr))

            :else ; atomic — limiting case
            expr)))

  (defn! transform
    [[clojure.walk :as walk]]
    ^{:doc "Expands the database function keywords to the database function calls *at the actual transaction time*,
            instead of e.g. just before the transaction is sent off.
            This enforces atomicity when the transaction needs to refer back to another part of the database.

            Essentially the example says, update person1’s song:preference attribute to whatever song title is
            one that is popular *during the transaction*.
            This is opposed to getting a bunch of inconsistent snapshots of the db and performing queries on them,
            aggregating the results into a transaction, *before* it’s sent off to the db."
      :example
        (transact!
           [[:fn/transform
              {:db/id (tempid :db.part/test)
               :agent:person:name:last:maternal
                 '(:fn/ffirst
                    (:fn/q [:find ?surname
                            :where [_ :agent:person:name:last:surname ?surname]]))}]])}
    [db m]
    (->> m
         (walk/prewalk ; Get rid of weird ArrayLists in order to walk correctly
           (fn [x]
              (cond (and (not (coll? x))
                         (instance? java.util.List x))
                    (into [] x)
                    (and (not (coll? x))
                         (instance? java.util.Set x))
                    (into #{} x)
                    :else x)))
         (datomic.api/invoke db :fn/eval db)
         #_(#(doto % (println "IS AFTER EVALUATION")))
         vector))
  ))
