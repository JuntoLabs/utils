(ns junto-labs.utils.db
           (:require [clojure.string        :as str]
                     [junto-labs.utils.core :as u
                       :refer [->ex fn-not nnil? nempty? ffilter
                               #?@(:clj [fn->> fn-> <- if-cljs])]]
                     [datascript.core       :as mdb]
             #?(:clj [datomic.api           :as db ]))
  #?(:cljs (:require-macros
                     [junto-labs.utils.core
                       :refer [fn->> fn-> <-]])))

; TODO: all of this code is in quantum... don't need to maintain two versions if we just use quantum, but it's okay for now  

; ===== DEFENTITY =====

(defonce schemas    (atom {}))
(defonce attributes (atom #{}))


(def identifier? #(or (keyword? %)
                      (integer? %)
                      #?(:clj (instance? datomic.db.DbId %))))

(def dbfn-call? #(and (seq? %)
                      (-> % first keyword?)
                      (-> % first namespace (= "fn"))))

(def lookup? #(and (vector? %)
                   (-> % first keyword?)))

(def ^{:doc "See also |quantum.db.datomic.core/allowed-types|."}
  validators
  {:keyword keyword?
   :string  string?
   :boolean #?(:clj  (partial instance? Boolean)
               :cljs #(or (true? %) (false? %)))
   :long    #?(:clj (partial instance? Long) #_long?  
               :cljs integer?) ; TODO CLJS |long?|
   :bigint  #?(:clj #(or (instance? BigInteger          %)
                         (instance? clojure.lang.BigInt %))
               :cljs integer?) ; TODO CLJS |bigint?|
   :float   #?(:clj float?  :cljs number? ) ; TODO CLJS |float?|
   :double  #?(:clj (partial instance? Double)     :cljs number?) ; TODO CLJS |number?|
   :bigdec  #?(:clj (partial instance? BigDecimal) :cljs number?) ; TODO CLJS |bigdec?|
   :ref     #(or (map? %) (dbfn-call? %) (identifier? %) (lookup? %)) ; Can be any entity/record
   ; TODO add these in
   ;:instant #?(:clj instant?)
   ;:uuid    #?(:clj uuid?)
   #?(:clj :uri)     #?(:clj (partial instance? java.net.URI))
   ;:bytes   #?(:clj bytes? :cljs bytes?)
   })


(defn keyword->class-name [k]
  (->> k name 
       (<- str/split #"\:")
       (map str/capitalize)
       (str/join "_")
       (<- str/replace "+" "AND")
       (<- str/replace "*" "_STAR_")
       (<- str/replace "-" "__")
       symbol))

(defn attr->constructor-sym [k]
  (symbol (str "->" (name k))))

(defn ->ref-tos-constructors
  "Extract the ref-tos constructors from an options map"
  [opts]
  (when-let [ref-to-0 (:ref-to opts)]
    (if (coll? ref-to-0)
        (map attr->constructor-sym ref-to-0)
        [(attr->constructor-sym ref-to-0)])))

#?(:clj
(defmacro defattribute
  "Defines a function which creates a Datomic attribute-value pair.
   
   Also adds the schema into the in-memory schema store
   when it defines this fn."
  {:example '(defattribute :agent
               [:ref :one {:ref-to #{:agent:person :agent:organization}}])}
  [attr-k schema]
  (let [[type cardinality opts] (eval schema)
        attribute-sym (-> attr-k name symbol)
        v-0         (gensym 'v-0)
        v-f         (gensym 'v-f)
        schema-eval (gensym 'schema-eval)
        opts-f      (dissoc opts :validator :transformer :ref-to)
        schema-f    [type cardinality opts-f]
        ref-tos     (->ref-tos-constructors opts)
        class-name  (-> attr-k keyword->class-name)
        constructor-name (symbol (str "->" (name attr-k)))]
    `(do (swap! schemas assoc ~attr-k ~schema-f)
         (swap! attributes conj ~attr-k)
         ~(list 'defrecord class-name ['v])
         (defn ~constructor-name [~v-0]
           (u/log ::debug "Constructing" '~class-name "with" (type ~v-0) ~v-0 "...")
           (cond
               (or (instance? ~class-name ~v-0)
                   (and ~(= type :ref) (identifier? ~v-0))
                   (lookup? ~v-0)
                   (dbfn-call? ~v-0)
                   (nil? ~v-0))
               ~v-0
               :else
               (let [~v-f (atom ~v-0)]
                 ~(if (= cardinality :many)
                      `(u/assert+ (every? (get validators ~type) (deref ~v-f)) #{~v-f})
                      `(u/assert+        ((get validators ~type) (deref ~v-f)) #{~v-f})) 
                 (when-let [transformer# (:transformer ~opts)]
                   (swap! ~v-f transformer#))
                 (when-let [validator#   (:validator   ~opts)] ; technically, post-transformer-validator 
                   (u/assert+ (validator# (deref ~v-f)) #{~v-f}))
                 
                 ~(when ref-tos
                    (let [valid-instance?
                           (apply list 'or
                             `(identifier? ~(list 'deref v-f))
                             (map #(list 'instance? % (list 'deref v-f)) ref-tos))
                          err-sym (gensym 'e)]
                      `(try (u/assert+ ~valid-instance? #{~v-f})
                         (catch Throwable ~err-sym
                           ~(if (-> ref-tos count (> 1))
                                `(throw ~err-sym)
                                (let [constructor (-> opts ->ref-tos-constructors first)]
                                   (if (= cardinality :many)
                                     `(swap! ~v-f
                                        (fn->> (map ~constructor)
                                               (into #{})))
                                     `(swap! ~v-f ~constructor))))))))
                 (new ~class-name (deref ~v-f)))))))))

#?(:clj
(defmacro defentity
  {:example '(defentity :media.agent+plays
               {:component? true}
               {:agent nil
                :media.plays
                  [:ref :many {:ref-to :time.instant :doc "Dates played"}]})}
  [attr-k & args]
  (let [args `~args
        [opts entity]
          (condp = (count args)
            1 [nil (eval (first args))] 
            2 [(eval (first args))
               (eval (second args))]
            (throw (->ex :illegal-argument "Invalid number of args" (count args))))
        fields
          (->> entity keys (mapv (fn-> name symbol)))
        class-name
          (-> attr-k keyword->class-name)
        class-name-fields
          (symbol (str (name attr-k) "___" "fields"))
        constructor-name (attr->constructor-sym attr-k)
        class-to-map
          (symbol (str "map->" (name class-name)))
        args-sym (gensym 'args)
        args (if (nempty? fields) [args-sym] [])
        destructured {:keys fields :as args-sym}
        ref-tos (->ref-tos-constructors opts)
        ;_ (println "/*" "INSIDE DEFENTITY AFTER REF TOS" "*/")
        m-f (gensym 'm-f)
        m-f* (gensym 'm-f*)
        code
          `(do (swap! schemas assoc ~attr-k [:ref :one ~opts])
               (defrecord ~class-name ~(conj fields 'type))
               (def ~class-name-fields (->> ~entity keys (into #{:db/id :db/ident})))
               (declare ~constructor-name)
               ~@(for [[k v] entity]
                   (when (nnil? v) `(defattribute ~k ~(update v 2 #(assoc % :component? true))))) ; assume all inline-declared attributes are components
               (defn ~constructor-name ~args
                 ; If alredy an instance of the class, return it
                 (if (or (instance? ~class-name ~args-sym)
                         (identifier? ~args-sym)
                         (lookup?     ~args-sym)
                         (dbfn-call?  ~args-sym))
                     ~args-sym
                     (let [~destructured ~args-sym]
                       (doseq [k# (keys ~args-sym)]
                         (assert (contains? ~class-name-fields k#)  #{k#}))
                       (let [~m-f (volatile! {})]
                         ; For each k, ensure the right constructor is called
                         ~@(for [k (->> entity keys)]
                            `(when (nnil? ~(-> k name symbol))
                               (let [v-f# (~(attr->constructor-sym k) ~(-> k name symbol))]
                                 (vswap! ~m-f assoc ~k v-f#))))
                         (vswap! ~m-f assoc :type ~attr-k) ; was having problems with transient
                         (when-let [id# (:db/id ~args-sym)]
                           (vswap! ~m-f assoc :db/id id#))
                         (when-let [ident# (:db/ident ~args-sym)]
                           (vswap! ~m-f assoc :db/ident ident#))
                         (~class-to-map (deref ~m-f)))))))
        ;_ (println "/*" "DEFENTITY CODE" code "*/")
        ]
    code)))

; Datomic will cache queries, so long as the query (first) argument data structures evaluate as equal. 
; reusing parameterized queries is much more efficient than building different query data structures.
; If you need to build data structures at run time for query, you should do so using a standard process
; so that equivalent queries will evaluate as equal.

(def attribute? #(and (:v %) (-> % count (= 1))))

(defn validated->txn
  "Transforms a validated e.g. record into a valid Datomic/DataScript transaction
   function.
   Assumes nested maps are component entities."
  [x]
  (if (record? x)
      (clojure.walk/postwalk
        (fn [x]
          (if (and (record? x) #?(:clj (not (instance? datomic.db.DbId x))))
              (if (attribute? x)
                  (:v x)
                  (->> x
                       (remove (fn [[k v]] (nil? v)))
                       (into {})))
              x))
        x)
      x))

#_(defn validated->txns
  "Transforms a validated e.g. record into a valid Datomic/DataScript transaction
   function.
   Assumes nested maps are separate entities which need to be transacted separately."
  [x]
  ...)

(defn queried->maps [db queried]
  (map #(->> % first (mdb/entity db) (into {})) queried))

; Optimally one could have a per-thread binding via dynamic vars, but
; based on certain tests, somehow they don't work 100% in ClojureScript.
; So we can go with atoms for now
(defonce db*   (atom nil))
(defonce conn* (atom nil))
(defonce part* (atom :db.part/user))

(defn unhandled-type [type obj]
  (condp = type
    :conn   (->ex :unhandled-predicate
                  "Object is not conn or db"
                  obj)
    :db     (->ex :unhandled-predicate
                  "Object is not mem-db or db"
                  obj)
    :entity (->ex :unhandled-predicate
                  "Object is not mem-entity or entity"
                  obj)))

(defn ->uri-string
  [{:keys [type host port db-name]
    :or {type    :free
         host    "localhost"
         port    4334
         db-name "test"}}]
  (str "datomic:" (name type) "://" host ":" port "/" db-name
       "?" "h2-port"     "=" (-> port inc)
       "&" "h2-web-port" "=" (-> port inc inc)))

#?(:clj (def db? (partial instance? datomic.db.Db)))

(def ^{:doc "'mdb' because checks if it is an in-*mem*ory database."}
  mdb? (partial instance? datascript.db.DB))

#?(:clj (def conn? (partial instance? datomic.Connection)))
(defn mconn? [x] (and (u/atom? x) (mdb? @x)))

(defn ->db
  "Arity 0: Tries to find a database object in the global variables.
   Arity 1: Tries to coerce @arg to a database-like object"
  ([]
    (let [db*-f   @db*
          conn*-f @conn*]
      (or db*-f
          (->db conn*-f))))
  ([arg]
    (cond (mconn? arg)
            @arg
 #?@(:clj [(conn? arg)
            (db/db arg)])
          :else
            (throw (->ex nil "Object cannot be transformed into database" arg)))))

#?(:clj
(defn ->conn
  "Creates a connection to a Datomic database."
  [uri] (db/connect uri)))

(defn q
  ([query] (q query (->db)))
  ([query db & args]
    (cond           (mdb? db) (apply mdb/q query db args)
          #?@(:clj [(db?  db) (apply db/q  query db args)])
          :else (throw (unhandled-type :db db)))))

(defn entity
  ([eid] (entity (->db) eid))
  ([db eid]
    (cond           (mdb? db) (mdb/entity db eid)
          #?@(:clj [(db?  db) (db/entity  db eid)])
          :else (throw (unhandled-type :db db)))))

(defn transact!
  ([tx-data]      (transact! @conn* tx-data))
  ([conn tx-data] (transact! conn tx-data nil))
  ([conn tx-data tx-meta]
    (let [txn (cond           (mconn? conn) (mdb/transact! conn tx-data tx-meta)
                    #?@(:clj [(conn?  conn) @(db/transact   conn tx-data)])
                    :else (throw (unhandled-type :conn conn)))]
      txn)))

(defn tempid
  ([] (tempid @part*))
  ([part] (tempid @conn* part))
  ([conn part]
    (assert (nnil? part))
    (cond            (mconn? conn) (mdb/tempid part)
          #?@(:clj  [(conn?  conn) (db/tempid  part)])
          :else (throw (unhandled-type :conn conn)))))

#?(:clj (def tempid? (partial instance? datomic.db.DbId)))

#?(:clj (def tempid-like? #(or (tempid? %) (integer? %))))

#_(do (when-let [conn @conn*] (db/release conn))
      (reset! conn* (-> {:host "52.71.47.150" :db-name "junto-labs" :port 4337} ->uri-string ->conn)))

; ===== SCHEMAS ===== ;

(def allowed-types
 #{:keyword
   :string 
   :boolean
   :long
   :bigint 
   :float
   :double 
   :bigdec 
   :ref
   :instant
   :uuid
   :uri
   :bytes})

(defn ->schema
  "Defines, but does not transact, a new database schema.
   Takes the pain out of schema creation."
  {:usage '(->schema :person.name/family-name :string :one {:doc "nodoc"})}
  ([ident val-type cardinality]
    (->schema ident val-type cardinality nil))
  ([ident val-type cardinality {:keys [conn part] :as opts}]
    (u/assert+ (contains? allowed-types val-type) #{val-type})
    (let [conn-f (or conn @conn*)
          part-f (when-not (mconn? conn-f)
                   (or part
                       :db.part/db))]
      ; Partitions are not supported in DataScript (yet)
      (when-not (mconn? conn-f)
        (u/assert+ (nnil? part-f) #{conn-f part-f}))
  
      (let [cardinality-f
              (condp = cardinality
                :one  :db.cardinality/one
                :many :db.cardinality/many
                (throw (->ex :unrecognized-cardinality  
                             "Cardinality not recognized:"
                             cardinality)))]
        (->> {:db/id                 (when-not (mconn? conn)
                                       (tempid part-f))
              :db/ident              ident
              :db/valueType          (keyword "db.type" (name val-type))
              :db/cardinality        cardinality-f
              :db/doc                (:doc        opts)
              :db/fulltext           (:full-text? opts)
              :db/isComponent        (:component? opts)
              :db/unique             (when (:unique opts)
                                       (->> opts :unique name
                                            (str "db.unique/") keyword))
              :db/index              (:index?     opts)
              :db.install/_attribute part-f}
             (filter (fn [[k v]] (nnil? v)))
             (into {}))))))

(defn block->schemas
  "Transforms a schema-block @block into a vector of individual schemas."
  {:usage '(block->schemas
             {:todo/text       [:string  :one]
              :todo/completed? [:boolean :one {:index? true}]
              :todo/id         [:long    :one {:index? true}]})}
  [block & [opts]]
  (->> block
       (mapv (fn [[ident [val-type cardinality opts-n]]]
               (->schema ident val-type cardinality
                 (merge {} opts opts-n))))))

(def has-transform? #(and (vector? %) (-> % first (= :fn/transform))))

(defn wrap-transform [x]
  #?(:clj  (if (has-transform? x)
                x
                [:fn/transform x])
     :cljs x))

(def transform (fn-> validated->txn wrap-transform))

(defn db-conj
  "Creates, but does not transact, an entity from the supplied attribute-map."
  {:todo ["Determine whether :fn/transform can be elided or not to save transactor time"]}
  ([m]      (db-conj @conn* @part* false m))
  ([part m] (db-conj @conn* part false m))
  ([conn part no-transform? m]
    (let [txn (u/assoc-when-none (validated->txn m) :db/id (tempid conn part))]
      (if no-transform? txn (wrap-transform txn)))))

(defn db-conj!
  "Creates and transacts an entity from the supplied attribute-map."
  ([& args]
    (transact! [(apply db-conj args)])))

(defn db-assoc
  "Transaction function which asserts the attributes (@kvs)
   associated with entity id @id."
  {:todo ["Determine whether :fn/transform can be elided or not to save transactor time"]}
  [eid & kvs]
  (wrap-transform
    (apply hash-map :db/id eid kvs)))

(defn db-merge
  "Merges in @props to @eid."
  [eid props]
  (-> props (assoc :db/id eid) validated->txn wrap-transform))

(defn db-merge!
  "Merges in @props to @eid and transacts."
  ([& args]
    (transact! [(apply db-merge args)])))

(defn db-dissoc
  "Transaction function which retracts the attributes (@kvs)
   associated with entity id @id.

   Unfortunately requires that one knows the values associated with keys.

   '|Retract| with no value supplied is on our list of possible future
    enhancements.' — Rich Hickey"
  {:todo ["Determine whether :fn/transform can be elided or not to save transactor time"]}
  [arg & args]
  (let [db-like? #(or (mdb? %) #?(:clj (db? %)) (mconn? %) #?(:clj (conn? %)))
        [db eid kvs] (if (db-like? arg)    
                         [(->db arg) (first args) (rest args)]
                         [(->db)     arg          args       ])
        retract-fn (if (mdb? db)
                       :db.fn/retractEntity
                       :db/retract)]
    [:fn/transform (concat (list retract-fn eid) kvs)]))

(defn db-dissoc! [& args]
  (transact! [(apply db-dissoc args)]))

(defn db-disj
  ([eid] (db-disj @conn* eid))
  ([conn eid]
    (wrap-transform `(:db.fn/retractEntity ~eid))))

(defn db-disj! [& args]
  (transact! [(apply db-disj args)]))

(defn lookup [a v]
  (ffirst (q [:find '?e :where ['?e a v]])))

; ===== DATABASE+TRANSACTION FUNCTIONS ===== ;

#?(:clj
(defmacro dbfn
  "Used for defining, but not transacting, a database function.

   Is not supported by DataScript."
  [requires arglist & body]
  `(db/function
     '{:lang     :clojure
       :params   ~arglist
       :requires ~requires
       :code     (do ~@body)})))

#?(:clj
(defmacro defn!
  "Used for defining and transacting a database function.

   Is not supported by DataScript."
  {:usage '(defn! inc [n] (inc n))}
  [sym requires arglist & body]
  `(db-conj! @conn* :db.part/fn true
     {:db/ident (keyword "fn" (name '~sym))
      :db/fn    
        (dbfn ~requires ~arglist ~@body)})))

(defn update-schema!
  {:usage '(update-schema! :task:estimated-duration :db/valueType :db.type/long)}
  [schema & kvs]
  (transact! [[:fn/transform
                (merge
                  {:db/id               schema
                   :db.alter/_attribute :db.part/db}
                  (apply hash-map kvs))]]))

(defn new-if-not-found
  "Creates a new entity if the one specified by the key-value pairs, @attrs,
   is not found in the database."
  [attrs]
  (let [query (into [:find '?e :where]
                (for [[k v] attrs]
                  ['?e k v]))]
  `(:fn/or
     (:fn/fq ~query)
     ~(tempid))))

(defn transact-all-schemas! []
  (-> @schemas block->schemas transact!))

#?(:clj
(defn entity-history [e]
  (db/q
    '[:find ?e ?a ?v ?tx ?added
      :in $ ?e
      :where [?e ?a ?v ?tx ?added]]
    (db/history (->db @conn*))
    e)))

#?(:clj
(defn entity-history-by-txn
  {:usage '(entity-history-by-txn (lookup :task:short-description :get-all-project-cljs))}
  [e]
  (->> (entity-history e)
       (map #(update % 1 (fn-> entity :db/ident)))
       (group-by #(get % 3))
       (into (sorted-map))
       (map val))))