;; # Templates
(ns map-graph-ea.template
  (:require [hyperfiddle.rcf :refer [tests]]
            [clojure.walk :refer [prewalk]]
            [malli.core :as mc]
            [malli.error :as me]
            [malli.registry :as mr]
            [malli.transform :as mt]
            [meander.epsilon :as m])
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
           
   ::directive [:and 
                [:map-of :keyword any?]
                [:orn
                 [:get ::get] 
                 [:each ::each]]]
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
                        ; Nexted objects
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
                         :%> "(fn [idx _] mod idx 1)"
                         :<= {0 ["li" {:? :value}]
                              1 ["li.odd" {:? :value}]}}})
                        ;
                         
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
  (m/rewrite
   expr

   {:<= {& (m/seqable [!k !t] ...)} & (m/cata ?rest)}
   {::rows {& [[!k (m/cata !t)] ...]}
    & ?rest}
   
   {:#> {& (m/seqable [!k !p] ...)} & (m/cata ?rest)}
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

   [!xs ...]
   [(m/cata !xs) ...] 

   ?x
   ?x))

(defn interpret-template
  [expr env]
  (m/match
   [expr env]

   (m/and
    [{:?* ?path} ?env]
    (m/let [?env-sym (gensym "_env_")]))
   (let [?env-sym (m/cata [{:? ?path} ?env])]
     ?env-sym)
   
   [{:? ?x} ?env]
   (if (vector? ?x)
     (get-in ?env ?x) 
     (get ?env ?x))))

(defn emitter
  [template]
  (fn [ctx]
    (when template
      (let [t (parse-template template)
            expand (fn [node]
                     (if (and (map? node) (= (count node) 1) (#{:? "?"} (ffirst node)))
                       (let [[[_ path]] (seq node)
                             e (emitter (if (seq? path)
                                          (get-in ctx path)
                                          (get ctx path)))]
                         (e ctx))
                       node))]
        (prewalk expand t)))))

(defn emit
  [content ctx]
  ((emitter content) ctx))
  

(tests
 (emit 1 {}) := 1
 (emit {:? :foo} {:foo 2}) := 2
 (emit {"?" "foo"} {:foo 2}) := 2
 (emit [1 2] {}) := [1 2]
 (emit {:a 1} {}) := {:a 1}
 (emit {:a {:? :foo} :b {:? :bar}} {:foo 3}) := {:a 3 :b nil}
 (emit {:a {:? :bar}} {:foo 4 :bar {:? :foo}}) := {:a 4})
