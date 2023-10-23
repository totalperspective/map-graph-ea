;; # Templates
(ns map-graph-ea.template
  (:require [hyperfiddle.rcf :refer [tests]]
            [malli.core :as mc]
            [malli.error :as me]
            [malli.registry :as mr]
            [malli.transform :as mt]
            [meander.epsilon :as m]
            [sci.core :as sci])
  (:import [java.lang Exception]))

(def spec
  {::directive-key [:orn
                    [:get [:= :?]]
                    [:for [:= :?*]]
                    [:row [:= :<=]]
                    [:index-fn [:= :%>]]
                    [:select [:= :=>]]
                    [:rename [:= :#>]]]
   ::property [:and
               :keyword
               [:not ::directive-key]]
   ::scalar [:or :string :int :double :boolean]
   ::path [:orn [:keyword :keyword] [:path [:vector :keyword]]]
   ::get [:map
          {:closed true}
          [:? {:title "value"} ::path]]
   ::row-test [:orn
               [:index :int]
               [:name :keyword]]
   ::each [:map
           {:closed true}
           [:?* {:title "list"} ::path]
           [:<= {:title "row option"} [:map-of ::row-test ::template]]
           [:%> {:optional true :title "indexFuntion"} :string]
           [:=> {:optional true :title "select"} :string]
           [:#> {:optional true :title "rename"} [:map-of ::property ::path]]]
           ;

   ::invoke [:map
             {:closed true}
             [:! {:title "invoke"} ::path]
             [:!? {:optional true} [:map-of ::property [:schema [:ref ::template]]]]]
   ::directive [:and
                [:map-of :keyword any?]
                [:orn
                 [:get ::get]
                 [:each ::each]
                 [:invoke ::invoke]]]
                 ;

   ::template-expr [:orn
                    [:scalar ::scalar]
                    [:directive ::directive]]
   ::template [:orn
               [:expr [:schema [:ref ::template-expr]]]
               [:object [:map-of ::property [:schema [:ref ::template]]]]
               [:array [:vector [:schema [:ref ::template]]]]]})

(def registry
  (merge
   (mr/schemas mc/default-registry)
   {:neg-int (mc/-simple-schema {:type :neg-int, :pred neg-int?})
    :pos-int (mc/-simple-schema {:type :pos-int, :pred pos-int?})}
   spec))

(defn schema
  [spec]
  (mc/schema spec {:registry registry}))

(def Template (schema ::template))

(defn valid-template?
  [query]
  (mc/validate Template query))

(defn parse-template
  [form]
  (let [query (mc/decode Template form mt/json-transformer)]
    (if (valid-template? query)
      query
      (let [error (mc/explain Template query)
            message (me/humanize error)]
        (throw (ex-info "Invalid Template"
                        {:error message
                         :detail (me/error-value error {::me/mask-valid-values '...})
                         :full error}))))))

^{:nextjournal.clerk/auto-expand-results? true
  :nextjournal.clerk/no-cache true}
(tests
 "parsing templates"
 (def template-forms '#{; Constants
                        "string"
                        true false
                        1 1.2
                        ; Arrays
                        ["foo" "bar"]
                        ; Objects
                        {:foo "bar"}
                        ; Nested objects
                        {:foo "bar"
                         :baz {:a {:b "c"}}}
                        ; Directives
                        ; Get
                        {:? :key}
                        {:? []} ; current value
                        {:? [:path :to :key]}
                        ; Nested Get
                        {:foo {:bar {:? [:val]}}}
                        ; Arrays
                        {:?* :key
                         :<= {:* {:? :value}}}
                        ; Select
                        {:?* []
                         :=> "#(:x %)"
                         :<= {:* [{:? :x} {:? :y}]}}
                        ; Rename
                        {:?* []
                         :#> {:x :a
                              :y [:b :c]}
                         :<= {:* [{:? :x} {:? :y}]}}
                        ; Header & Footer
                        {:?* [:key]
                         :<=  {:< ["li.head" {:? []}]
                               :. ["li" {:? []}]
                               :> ["li.odd" {:? []}]}}
                        ; Alt index fn
                        {:?* []
                         :%> "(fn [idx _] (mod idx 1))"
                         :<= {0 ["li" {:? :value}]
                              1 ["li.odd" {:? :value}]}}
                        ; Invoke
                        {:! {:? :fn}
                         :!? {:my-var {:? [:path :to :var]}}}})

 (->> template-forms
      (map (fn [form]
             (try
               (parse-template form)
               (catch Exception e
                 (or (ex-data e)
                     (str e))))))
      (into #{}))
 := template-forms)

^{:nextjournal.clerk/auto-expand-results? true
  :nextjournal.clerk/no-cache true}
(tests
 "parsing json templates"
 (def t->js
   (mc/encoder Template mt/json-transformer))
 (->> template-forms
      (map t->js)
      (map (fn [form]
             (try
               (parse-template form)
               (catch Exception e
                 (or (ex-data e)
                     (str e))))))
      (into #{}))
 := template-forms)

(defn parse-form
  [expr]
  #_{:clj-kondo/ignore [:syntax :unresolved-symbol :unresolved-var]}
  (m/rewrite
   expr
   {:!? {& (m/seqable [!k !t] ...)}
    & (m/cata ?rest)}
   {::env {& [[!k (m/cata !t)] ...]}
    & ?rest}

   {:! (m/some ?x)}
   {::tag :invoke
    ::fn (m/cata ?x)}

   {:<= {& (m/seqable [!k !t] ...)}
    & (m/cata ?rest)}
   {::rows {& [[!k (m/cata !t)] ...]}
    & ?rest}

   {:#> {& (m/seqable [!k !p] ...)}
    & (m/cata ?rest)}
   {::env {& [[!k (m/cata {:? !p})] ...]}
    & ?rest}

   {:=> (m/some ?fn) & (m/cata ?rest)}
   {::select-fn ?fn
    & ?rest}

   {:%> (m/some ?fn) & (m/cata ?rest)}
   {::index-fn ?fn
    & ?rest}

   {:?* (m/some [!xs ...]) & (m/cata ?rest)}
   {::tag :list
    ::path [!xs ...]
    ::meta ?rest}

   {:?* (m/some ?x) & (m/cata ?rest)}
   {::tag :list
    ::path [?x]
    ::args ?rest}

   {:? [!xs ...]}
   {::tag :get
    ::path [!xs ...]}

   {:? (m/some ?x)}
   {::tag :get
    ::path [?x]}

   (!xs ...)
   [(m/cata !xs) ...]

   [!xs ...]
   [(m/cata !xs) ...]

   {& (m/seqable [!k !v] ...)}
   {& [[!k (m/cata !v)] ...]}

   ?x
   ?x))

#_{:clj-kondo/ignore [:syntax :unused-binding]}
(defn interpret-template
  [expr env]
  (when (instance? Exception env)
    (throw env))
  (when (instance? Exception expr)
    (throw expr))
  (tap> expr)
  (let [result (m/match
                [expr env]
                 [(m/and (m/pred keyword?) ?k) ?env]
                 (do
                   (tap> {:keyword ?k})
                   ?k)

                 [{::tag :invoke ::fn (m/some ?fn) ::env ?extra-env} ?env]
                 (let [fn (interpret-template ?fn ?env)
                       env (into ?env (interpret-template ?extra-env ?env))]
                   (tap> {:?fn ?fn :extra-env ?extra-env})
                   (if (fn? fn)
                     (try
                       (fn env)
                       (catch Exception e
                         e))
                     (ex-info "Expression did not yeild a function"
                              {:expr ?fn
                               :actual fn})))
                 (m/and
                  [{::tag :list
                    ::path ?path
                    ::env ?rename
                    ::select-fn ?select-str
                    ::index-fn ?index-str
                    ::rows ?rows} ?env]
                  (m/let [?array-sym (gensym "_array_")
                          ?count-sym (gensym "_count_")]))
                 (let [?array-sym (get-in ?env ?path)]
                   (if (sequential? ?array-sym)
                     (let [?count-sym (when (counted? ?array-sym)
                                        (count ?array-sym))]
                       (transduce (comp
                                   (filter (if ?select-str
                                             (sci/eval-string ?select-str)
                                             (constantly true)))
                                   (map (fn [item]
                                          (interpret-template (parse-form item) ?env)))
                                   (map (if (map? ?rename)
                                          (fn [row]
                                            (reduce-kv
                                             (fn [m k v]
                                               (tap> [m k v])
                                               (assoc m k (interpret-template v row)))
                                             {}
                                             ?rename))
                                          identity))
                                   (map-indexed (let [idx-fn (if ?index-str
                                                               (sci/eval-string ?index-str)
                                                               (fn [idx _] idx))]
                                                  (fn [idx item]
                                                    #_(prn idx item)
                                                    {:row idx
                                                     :item item
                                                     :idx (idx-fn idx item)
                                                     :max (when ?count-sym (dec ?count-sym))})))
                                   (map (fn [{:keys [idx max item] :as m}]
                                          (if ?rows
                                            (let [index (if ?index-str
                                                          idx
                                                          (cond
                                                            (= 0 idx) :<
                                                            (= max idx) :>
                                                            :else :.))
                                                  template (or (get ?rows idx)
                                                               (get ?rows index)
                                                               (:* ?rows))]
                                              (interpret-template template (assoc m :.. ?env :. item)))
                                            item))))
                                  conj
                                  ?array-sym))
                     []))

                 [{::tag :get ::path ?path} ?env]
                 (let [val (get-in ?env ?path)
                       result (interpret-template (parse-form val) ?env)]
                   (tap> {:get ?path :result val :final result})
                   (when (not= result :com.wsscode.pathom3.error/attribute-unreachable)
                     result))

                 [[!exprs ...] ?env]
                 (transduce (map #(interpret-template % ?env))
                            conj
                            !exprs)

                 [{& ?rest} ?env]
                 (reduce-kv (fn [m k v]
                              (assoc m k (interpret-template v ?env)))
                            {}
                            ?rest)

                 [?expr ?env]
                 ?expr)]
                 (when (instance? Exception result)
                   (throw result))
                 (tap> {:fn 'interpret-template :expr expr :env env :result result})
                 result))

(defn emitter
  [content]
  (let [template (-> content
                     parse-template
                     parse-form)]
    (fn [ctx]
      (interpret-template template ctx))))

(defn emit
  [content ctx]
  ((emitter content) ctx))

^{:nextjournal.clerk/auto-expand-results? true
  :nextjournal.clerk/no-cache true}
(tests
 (emit 1 {}) := 1
 (emit "string" {}) := "string"
 (emit {:? :foo} {:foo 2}) := 2
 (emit {"?" "foo"} {:foo 2}) := 2
 (emit [1 2] {}) := [1 2]
 (emit {:a 1} {}) := {:a 1}
 (emit {:a {:? :foo} :b {:? :bar}} {:foo 3}) := {:a 3 :b nil}
 (emit {:a {:? :bar}} {:foo 4 :bar {:? :foo}}) := {:a 4}
 (emit {:?* [:list]
        :%> "(fn [idx _] (mod idx 2))"
        :<= {0 ["li" {:? [:.]}]
             1 ["li.odd" {:? [:.]}]}}
       {:list [:first :second :third]})
 := [["li" :first] ["li.odd" :second] ["li" :third]]
 (emit {:?* [:list]
        :<= {:* {:? [:.]}}}
       {:list [:first :second :third]}) := [:first :second :third]
 (emit {:?* [:list]
        :<= {:* {:? [:.]}}}
       {:list :not-a-list}) := []
 (emit {:! {:? [:fn]}}
       {:fn #(get % :var) :var 1}) := 1
 (emit {:! {:? :inc}
        :!? {:num {:? :value}}}
       {:inc #(-> %
                  :num
                  inc)
        :value 1}) := 2)
