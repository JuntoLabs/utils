(ns ^{:cljs-self-referring? true}
  junto-labs.utils.core
           (:require [clojure.string               :as str  ]
                     [clojure.walk                 :as walk
                       :refer [postwalk]                    ]
             #?(:clj [postal.core                  :as email])
            #?(:clj  [clojure.edn                  :as edn  ])
            #?(:clj  [clojure.repl                 :as repl ])
                     [#?(:clj  clojure.core.async
                         :cljs cljs.core.async   ) :as async
                       :refer [chan offer! take!
                               #?@(:clj [go go-loop])]]
                     [#?(:clj  clj-http.client
                         :cljs cljs-http.client)   :as http  ]
            #?(:cljs [cljs.core :refer [ExceptionInfo]]))
  #?(:cljs (:require-macros
                     [junto-labs.utils.core
                       :refer [defc assert+]]))
  #?(:clj  (:import clojure.lang.ExceptionInfo)))

(declare ->ex)

; ===== TYPE PREDICATES ===== ;

(defn atom? [x]
  #?(:clj  (instance?  clojure.lang.IAtom x)
     :cljs (satisfies? cljs.core/IAtom    x)))

(def error? #(instance? #?(:clj Throwable :cljs js/Error) %))

#?(:cljs
(defn e-value [e]
  (-> e .-target .-value)))

; ===== COLLECTIONS ===== ;

(def lasti #(-> % count dec))

#?(:clj
(defmacro <-
  ([x] `(~x))
  ([cmd & body]
      `(~cmd ~(last body) ~@(butlast body)))))

(defn map-vals [f coll]
  (map (fn [[k v]] [k (f v)]) coll))

(defn map-keys [f coll]
  (map (fn [[k v]] [(f k) v]) coll))

(defn filter-vals [pred coll]
  (filter (fn [[k v]] (pred v)) coll))

(defn filter-keys [pred coll]
  (filter (fn [[k v]] (pred k)) coll))

(defn remove-vals [pred coll]
  (remove (fn [[k v]] (pred v)) coll))

(defn remove-keys [pred coll]
  (remove (fn [[k v]] (pred k)) coll))

(defn update-vals
  {:usage '(update-vals (p update-vals inc)
             {:c {} :a {:b 1}})}
  [f coll]
  (into {} (map (fn [[k v]] [k (f v)]) coll)))

(defn sort-vals-by
  {:usage '(sort-vals-by count {:a [1 2 4] :b [1 2] :c [1 1 1 1]})}
  [f m]
  (into (sorted-map-by (fn [key1 key2]
                        (compare [(f (get m key2)) key2]
                                 [(f (get m key1)) key1])))
       m))

(defn sort-vals
  {:usage '(sort-vals {:a 1 :b 2 :c 3})}
  [m]
  (sort-vals-by identity m))

(defn update-key
  {:usage '(update-key {:people {:a {:count 1}}}
            :people add-name)}
  [m k f]
  (assoc m k (f (k m))))


(defn remove-index [i coll]
  (concat (take i coll)
          (drop (inc i) coll)))

(def kvseq->map #(reduce conj {} %))

(def nempty? (complement empty?))
(def nnil? (complement nil?))

(def ffilter (comp first filter))

(defn assoc-when-none 
  "assoc's @args to @m only when the respective keys are not present in @m."
  [m & args]
  (reduce
    (fn [m-f [k v]]
      (if (contains? m-f k)
          m-f
          (assoc m-f k v)))
    m
    (partition-all 2 args)))

(defn distinct-by
  "Returns a lazy sequence of the elements of coll, removing any elements that
  return duplicate values when passed to a function f."
  {:attribution "medley.core"}
  [f coll]
  (let [step (fn step [xs seen]
               (lazy-seq
                ((fn [[x :as xs] seen]
                   (when-let [s (seq xs)]
                     (let [fx (f x)]
                       (if (contains? seen fx) 
                         (recur (rest s) seen)
                         (cons x (step (rest s) (conj seen fx)))))))
                 xs seen)))]
    (step coll #{})))


(defn postwalk-pred
  "Applies |postwalk| to @coll until an element of @coll is found to satisfy @pred.
   Returns true if satisfies, and false if not."
  [pred coll]
  ; Not the most elegant way to stop a postwalk prematurely, but effective
  (try
    (postwalk
      (fn [x]
        (if (pred x)
            (throw (->ex :early-termination))
            x))
      coll)
    false
    (catch ExceptionInfo e
      (if (-> e ex-data :type (= :early-termination))
          true
          (throw e)))))

#?(:clj
(defmacro kmap [& ks]
 `(zipmap (map keyword (quote ~ks)) (list ~@ks))))

(defn name+ [x] (when (nnil? x) (name x)))

; ===== META ===== ;

#?(:clj
(defn d [x]
  (clojure.repl/find-doc x)))

; ===== LOGICAL/CONTROL ===== ;

#?(:clj
(defmacro while-let
  "Repeatedly executes body while test expression is true, evaluating the body with binding-form bound to the value of test."
  {:source "markmandel/while-let"}
  [[form test] & body]
  `(loop [~form ~test]
       (when ~form
           ~@body
           (recur ~test)))))

; ===== FUNCTIONAL ===== ;

#?(:clj
(defmacro defc [name args & body]
  {:pre [(not-any? #{'&} args)]}
  (if (empty? args)
    `(defn ~name ~args ~@body)
    (let [rec-funcs (reduce (fn [l v]
                              `(letfn [(helper#
                                         ([] helper#)
                                         ([x#] (let [~v x#] ~l))
                                         ([x# & rest#] (let [~v x#]
                                                         (apply (helper# x#) rest#))))]
                                 helper#))
                            `(do ~@body) (reverse args))]
      `(defn ~name [& args#]
         (let [helper# ~rec-funcs]
           (apply helper# args#)))))))

#?(:clj
(defmacro fn->
  "Equivalent to |(fn [x] (-> x ~@body))|"
  {:attribution "thebusby.bagotricks"}
  [& body]
  `(fn [x#] (-> x# ~@body))))

#?(:clj 
(defmacro fn->>
  "Equivalent to |(fn [x] (->> x ~@body))|"
  {:attribution "thebusby.bagotricks"}
  [& body]
  `(fn [x#] (->> x# ~@body))))

(def p partial)

(def call (fn [x] (x)))

(def fn-not complement)

; ===== STRING ===== ;

(defc csplit [sub string]
  (str/split string sub))

; ===== MACROS ===== ;

(defn- cljs-env?
  "Given an &env from a macro, tells whether it is expanding into CLJS."
  [env]
  (boolean (:ns env)))

(defn if-cljs
  "Return @then if the macro is generating CLJS code and @else for CLJ code."
  {:from "https://groups.google.com/d/msg/clojurescript/iBY5HaQda4A/w1lAQi9_AwsJ"}
  ([env then else]
    (if (cljs-env? env) then else)))

; ===== CONVERSION ===== ;

#?(:clj
(defn makeInt [x]
  (condp instance? x
     Number x
     String (. Integer parseInt x))))

; ===== ERRORS/EXCEPTIONS ===== ;

(defrecord Err [type msg objs])

(defn ->err
  "Constructor for |Err|."
  ([type]          (if (map? type)
                       (map->Err type)
                       (Err. type nil nil)))
  ([type msg]      (Err. type msg nil))
  ([type msg objs] (Err. type msg objs)))

(defn ->ex
  "Creates an exception."
  ([type]          (ex-info (name type) (->err type)))
  ([type msg]      (ex-info msg  (->err type msg)))
  ([type msg objs] (ex-info msg  (->err type msg objs))))

; ===== VERIFICATION ===== ;

#?(:clj
(defmacro assert+
  "Like |assert|, but takes a type"
  {:references ["https://github.com/google/guava/wiki/PreconditionsExplained"]
   :usage '(let [a 4]
             (assert+ (neg? (+ 1 3 a)) #{a}))}
  [expr & [syms type]]
  `(if-let [expr# ~expr]
     expr#
     (throw
       (->ex ~(or type :assertion-error)
             (str "Assertion not satisfied: " '~expr ; TODO having this assertion string can be expensive if assertions fail on large data structures 
                   "\n"
                   "Symbols: " (kmap ~@syms))
             (assoc (kmap ~@syms)
               :assert-expr '~expr))))))

#?(:clj
(defmacro catch-asserts-with [catch-fn & try-exprs]
  `(try ~@try-exprs
     (catch ExceptionInfo e#
       (if (-> e# ex-data :type (= :assertion-error))
           (~catch-fn e#)
           (throw e#))))))

#?(:clj
(defmacro throw-unless
  "Throws an exception with the given content @throw-content if
   @expr evaluates to false.

   Specifically for use with :pre and :post conditions."
  {:attribution "Alex Gunnarson"}
  ([expr throw-content]
   `(let [expr# ~expr]
      (if expr# expr# (throw ~throw-content))))))

#?(:clj
(defmacro catch-all
  [expr]
  `(try ~expr
     (catch ~(if-cljs &env 'js/Error 'Throwable) e#))))

; ===== CACHING ===== ;

(defn memoize+
  "I have a fuller version in Quantum, but this was all we needed."
  [f & [{:keys [data get-fn txn-fn assoc-fn] :as opts}]]
  (let [get-fn   (or get-fn   (fn [data-n args  ] (get data-n args)))
        txn-fn   (or txn-fn   swap!)
        assoc-fn (or assoc-fn (fn [data-n args v-delay] (assoc data-n args @v-delay)))]
    (fn [& args]
      (let [v (delay (apply f args))]
        (txn-fn data
          (fn [data-n]
            (if (nil? (get-fn data-n args))
                (assoc-fn data-n args v)
                data-n)))))))

; ===== LOGGING ===== ;

(defonce log-levels (atom #{:severe :warn}))

#?(:clj
(defmacro log [level & args]
  `(when (contains? @log-levels ~level)
     (println ~@args)
     (flush))))

; ===== PARSING ===== ;

#?(:clj
(defn read-string+
  "Ensures that all forms in @s are read, not just the first one found."
  [s]
  (binding [*read-eval* false]
    (read-string (str "(do " s ")")))))

; ===== TIME ===== ;

(defn now-millis []
  #?(:clj  (System/currentTimeMillis)
     :cljs (js/Date.now)))

; ===== EMAIL ===== ;

; TODO move these elsewhere
(defonce email*    (atom nil))
(defonce password* (atom nil))
(defonce host*     (atom nil))
(defonce to*       (atom nil))

(defn email->username [email]
  (-> email (str/split #"@") first))

#?(:clj
(defn send-email!
  "Before using this on e.g. Google, you need to go to the following site and turn 'Less secure apps' on  
   https://www.google.com/settings/security/lesssecureapps
   https://support.google.com/accounts/answer/6010255?hl=en"
  {:example '(do (reset! u/email* "alexandergunnarson@gmail.com")
                 (reset! u/host*  "smtp.gmail.com")
                 (reset! u/to*    "cwhitesullivan@gmail.com")
                 (send-email-scoped! ...))}
  [{:keys [host username password from to subject body] :as opts}]
  (postal.core/send-message
    {:user     (assert+
                 (or username
                     (when-let [email @email*]
                       (email->username email))))
     :pass     (assert+ (or password @password*))
     :host     (assert+ (or host     @host*))
     :ssl      true}
    {:from     (assert+ (or from     @email*))
     :to       (assert+ (or to       @to*  ))
     :subject  subject
     :body     body})) ) 

; ===== HTTP ===== ;

(defn http=>chan
  "Abstracts the differences away from CLJS vs. CLJ HTTP requests."
  [req c]
  #?(:clj  (offer! c (http/request req))
     :cljs (take! (http/request req)
              (fn [resp] (offer! c resp)))))

; ===== ANALYZER ===== ;

(def unquote-form?
  #(and (seq? %) (-> % first (= 'clojure.core/unquote))))

(defn has-unquote? [form]
  (postwalk-pred unquote-form? form))

; ===== RESOURCES / LIFECYCLE ===== ;

(defprotocol Lifecycle
  (start [this])
  (stop  [this]))

; ===== UI ===== ;

#?(:cljs
(defn elem->bounds [elem]
  (let [bounds (.getBoundingClientRect elem)]
    ; TODO use js->clj?
    {:left   (.-left   bounds)
     :top    (.-top    bounds)
     :right  (.-right  bounds)
     :bottom (.-bottom bounds)
     :width  (.-width  bounds)
     :height (.-height bounds)})))