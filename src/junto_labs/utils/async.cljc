(ns ^{:cljs-self-referring? true}
  junto-labs.utils.async
  (:require [junto-labs.utils.core        :as u
              :refer [#?@(:clj [while-let]) nnil?]]
            [#?(:clj  clojure.core.async
                :cljs cljs.core.async   ) :as async
              :refer [chan promise-chan
                      #?@(:clj [go go-loop])
                      offer! poll! alts!
                      timeout
                      take! put! <! >!]            ])
  #?(:cljs (:require-macros
            [cljs.core.async.macros
              :refer [go go-loop]]
            [junto-labs.utils.async
              :refer [<!-timeout]]
            [junto-labs.utils.core :as u
              :refer [while-let]])))

#?(:clj
(defmacro <!-timeout [c & [timeout-n]]
  `(first (alts! [~c (timeout ~timeout-n)] :priority true))))

(defn capacity
  "Capacity of a channel"
  {:todo ["Get rid of reflection"]}
  [c]
  (-> c .buf .n))

(defn drain!
  "Takes from @c until no more values are left."
  [c]
  (while (async/poll! c)))

(defn drain-with!
  {:attribution "alexandergunnarson"}
  [f c]
  (let [results (transient [])]
    (while (when-let [result-n (async/poll! c)]
             (conj! results (f result-n))))
    (persistent! results)))

(defn transfer!*
  {:attribution "alexandergunnarson"}
  ([from] (transfer!* from (chan (capacity from))))
  ([from to]
    (let [ct (volatile! 0)]
      (while (let [taken (poll! from)]
               (when (nnil? taken)
                 (offer! to taken)))
        (vswap! ct inc))
    [to ct])))

(defn transfer!
  "Transfers all immediately available values from
   @from to an optional @to.
   Returns @to.
   Better named |into|, probably."
  {:attribution "alexandergunnarson"}
  ([from]    (first (transfer!* from   )))
  ([from to] (first (transfer!* from to))))

(defn take-chain!
  "Creates a chain of takes from the arguments.
   The first argument is a channel.
   The middle arguments are each channel-generators.
   The last (terminal) argument is a consumer of the last
   take-value.

   (take-chain! cache-promise
    :limit-chan (fn [_] (http->chan req result))
    resp->chan  (fn [x] (println x)))

   Expands to:
 
   (take! cache-promise
     (fn [ret]
       (take! (:limit-chan ret)
         (fn [ret]
           (take! ((fn [_] (http->chan req result)) ret)
             (fn [ret]
               (take! (resp->chan ret)
                 (fn [ret]
                   ((fn [x] (println x)) ret)))))))))"
  {:attribution "alexandergunnarson"}
  ([arg] arg)
  ([arg & args]
    (take! arg
      (fn [ret]
        (if (u/error? ret)
            (throw ret)
            (apply take-chain! ((first args) ret) (rest args)))))))

(defprotocol SequentialTransact
  (sswap!  [this f]
           [this f arg]
           [this f arg & args])
  (sreset! [this v-f]))

(deftype
  ^{:doc "Differences from atoms (rationale): 
            - Value  are sequential in the order in which they are
              requested to be executed.
            - The swap-fn does not need to be side-effect free since it
              is guaranteed to run only once.
          Similarities to atoms:
            - Non-blocking
          Disadvantages:
            - It requires a separate go-block to ensure sequentiality.
            - Around 10-30 times slower than an atom. So use only when you
              need it. (or else maybe I should just make this faster).
          Performance:
            11-34 ms:
              (let [a (atom 0)]
                (dotimes [n 100000] (swap! a inc)))
            229-339 ms just for the puts:
              (let [sval (->sval 0 100000)]
                (dotimes [n 10000] (sswap! sval inc)))
            No difference in performance as between |offer!| and |put!|"}
  SequentiallyTransactingValue
  [v txn-requests interrupted?]
  u/Lifecycle
    (start   [this] (reset! interrupted? false)
                    (go-loop []
                      (let [timeout-f (timeout 1000)]
                        (while-let [taken (first (alts! [txn-requests timeout-f] :priority true))]
                          (let [[resp-c f] taken
                                [err? ret] (try [false (f @v)]
                                             (catch #?(:clj Throwable :cljs js/Error) e
                                               [true e]))]
                            (when-not err? (reset! v ret))
                            (offer! resp-c ret))))
                      (if @interrupted?
                          (println "Sequential transactor interrupted.")
                          (recur)))
                    this)
    (stop    [this] (reset! interrupted? true))
  SequentialTransact
    (sswap!  [this f           ] (let [c (promise-chan)]
                                   (put! txn-requests [c f]) ; "no more than 1024 puts are allowed" could be a problem here
                                   c))
    (sswap!  [this f arg       ] (sswap! this #(      f % arg     )))
    (sswap!  [this f arg & args] (sswap! this #(apply f % arg args)))
    (sreset! [this v-f         ] (sswap! this (constantly v-f)))
  #?(:clj clojure.lang.IDeref
     :cljs cljs.core/IDeref)
    (deref   [this] @v))

(defn ->sval [v & [max-txn-requests]]
  (u/start (SequentiallyTransactingValue. (atom v) (atom false) (chan (or max-txn-requests 100)))))

(deftype RecurringTask
  [f every interrupted?]
  u/Lifecycle
    (start [this] (go-loop []
                    (f (u/now-millis))
                    (<! (timeout every))
                    (if @interrupted?
                        (println "Recurring task" f "interrupted.")
                        (recur))))
    (stop  [this] (reset! interrupted? true)))

(defn ->recurring-task [every f]
  (RecurringTask. f every (atom false)))