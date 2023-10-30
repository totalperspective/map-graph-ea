;; # Templates
(ns map-graph-ea.template
  (:require [hyperfiddle.rcf :refer [tests]]
            [malli.core :as mc]
            [malli.error :as me]
            [malli.registry :as mr]
            [malli.transform :as mt]
            [meander.epsilon :as m]
            [com.wsscode.pathom3.connect.runner :as pcr]
            [sci.core :as sci]
            [map-graph-ea.match :as mge.m])
  (:import [java.lang Exception]))

(defn ->list
  [x]
  ;; (prn x)
  (apply list x))

(def spec
  {::directive-key [:and
                    :keyword
                    [:orn
                     [:get [:= :?]]
                     [:get-raw [:= :??]]
                     [:for [:= :?*]]
                     [:row [:= :<=]]
                     [:index-fn [:= :%>]]
                     [:select [:= :=>]]
                     [:rename [:= :#>]]
                     [:hiccup [:= :<>]]
                     [:invoke [:= :!]]
                     [:match-pattern [:= :$?]]
                     [:match-expr [:= :$=]]]]
   ::property [:and
               :keyword
               [:not ::directive-key]]
   ::scalar [:or :string :int :double :boolean :nil]
   ::path-expr [:orn
                [:keyword :keyword]
                [:logic-variable ::logic-var]]
   ::path [:orn [:path-expr ::path-expr] [:path [:vector ::path-expr]]]
   ::get [:map
          {:closed true}
          [:? {:title "value"} ::path]]
   ::get-raw [:map
              {:closed true}
              [:?? {:title "value"} ::path]]
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
             [:! {:title "invoke"}
              [:and
               [:catn
                [:invoke-key :symbol]
                [:params [:? [:schema [:ref ::template]]]]]
               [list? {:decode {:json ->list}}]]]]
   ::hiccup-child-expr [:orn
                        [:string :string]
                        [:directive
                         [:schema [:ref ::directive]]]
                        [:hiccup [:schema [:ref ::hiccup-expr]]]
                        [:list
                         [:and
                          [:+ [:schema [:ref ::hiccup-expr]]]
                          [list? {:decode {:json ->list}}]]]]
   ::hiccup-expr [:and
                  [:catn
                   [:tag :keyword]
                   [:props [:? [:map-of ::property [:schema [:ref ::template]]]]]
                   [:children [:* ::hiccup-child-expr]]]
                  vector?]
   ::hiccup [:map
             {:closed true}
             [:<> ::hiccup-expr]]
   ::logic-var [:and
                :symbol
                [:fn (fn lvar? [v]
                       (and (symbol? v)
                            (re-matches #"^?.+$" (name v))))]]
   ::seq-pattern [:and
                  vector?
                  [:catn
                   [:pattern [:+ [:schema [:ref ::pattern]]]]
                   [:splat [:and :symbol [:= '...]]]]]
   ::pattern [:orn
              [:logic-variable ::logic-var]
              [:sequence-pattern ::seq-pattern]
              [:map-pattern [:map-of :keyword [:schema [:ref ::pattern]]]]]
   ::match [:map
            {:closed true}
            [:$? {:title "pattern"} ::pattern]
            [:$= {:title "rewrite"} [:schema [:ref ::template]]]]
   ::directive [:and
                [:map-of ::directive-key any?]
                [:orn
                 [:get ::get]
                 [:get-raw ::get-raw]
                 [:each ::each]
                 [:invoke ::invoke]
                 [:hiccup ::hiccup]
                 [:match ::match]]]
                 ;

   ::template-expr [:orn
                    [:scalar ::scalar]
                    [:directive ::directive]
                    [:logic-variable ::logic-var]]
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
(def Directive (schema ::directive))

(defn valid-template?
  [form]
  (mc/validate Template form))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn valid-directive?
  [form]
  (mc/validate Directive form))

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
                        ; Get Raw
                        {:?? :key}
                        {:?? []} ; current value
                        {:?? [:path :to :key]}
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
                        {:! (f 1)}
                        {:! (f {:? :var})}
                        ; Hiccup
                        {:<> [:div.class {:id "div-id"} "string"]}
                        {:<> [:ul [:li "1"] [:li "1"]]}
                        {:<> [:ul ([:li "1"] [:li "2"])]}
                        {:<> [:div {} [:my-input] "test"]}
                        ["span" {:class "red"} "world"]

                        ; Pattern Matching
                        {:$? {:key ?value}
                         :$= ?value}
                        {:$? {:key ?value}
                         :$= [?value]}
                        {:$? {:key ?value}
                         :$= {:? [:path :to ?value]}}

                        ;
                        })

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
  #_{:clj-kondo/ignore [:syntax :unresolved-symbol :unresolved-var :unused-binding]}
  (m/rewrite
   expr
   {:! ((m/and (m/pred symbol?)
               ?fn)
        & ?args)}
   {::tag :invoke
    ::fn ?fn
    ::args [& (m/cata ?args)]}

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
    ::meta ?rest
    ::index-fn nil
    ::select-fn nil
    ::env nil
    ::rows nil}

   {:?* (m/some ?x) & (m/cata ?rest)}
   {::tag :list
    ::path [?x]
    ::args ?rest
    ::index-fn nil
    ::select-fn nil
    ::env nil
    ::rows nil}

   {:? [!xs ...]}
   {::tag :get
    ::path [!xs ...]}

   {:? (m/some ?x)}
   {::tag :get
    ::path [?x]}

   {:?? [!xs ...]}
   {::tag :get-raw
    ::path [!xs ...]}

   {:?? (m/some ?x)}
   {::tag :get-raw
    ::path [?x]}

   {:<> [?tag]}
   (m/cata {:<> [?tag {}]})

   {:<> [?tag
         (m/and
          (m/or
           (m/not (m/pred map?))
           (m/pred valid-directive?))
          ?child)
         & ?children]}
   (m/cata {:<> [?tag {} ?child & ?children]})

   {:<> [?tag (m/and (m/pred map?) ?props) & ?children]}
   {::tag :hiccup
    ::element ?tag
    ::props (m/cata ?props)
    ::children (m/cata ?children)}

   {:$? (m/some ?pattern)
    :$= (m/some ?expr)}
   {::tag :match
    ::match ?pattern
    ::expr ?expr}

   (!xs ...)
   ((m/cata !xs) ...)

   [!xs ...]
   [(m/cata !xs) ...]

   {& (m/seqable [!k !v] ...)}
   {& [[!k (m/cata !v)] ...]}

   ?x
   ?x))

(def parse (comp parse-form parse-template))

(defn invoker
  [f]
  (fn [& args]
    (fn [_]
      (apply f args))))

#_{:clj-kondo/ignore [:syntax :unused-binding]}
(defn interpret-template
  [expr env]
  (when (instance? Exception env)
    (throw env))
  (when (instance? Exception expr)
    (throw expr))
  (tap> expr)
  (let [interpret-template* (fn [env expr]
                              (interpret-template expr env))
        result (m/match
                [expr env]
                 [{::pcr/attribute-errors (m/some ?error) & ?rest} ?env]
                 (do
                   (tap> {::error ?error})
                   (interpret-template ?rest ?env))

                 [(m/and (m/pred keyword?) ?k) ?env]
                 (do
                   (tap> {:keyword ?k})
                   ?k)

                 [{::tag :invoke ::fn (m/some ?fn) ::args ?args} ?env]
                 (let [f (get ?env ?fn)
                       args (mapv (partial interpret-template* ?env) ?args)
                       env ?env]
                   (tap> {:fn f :args args})
                   (if (fn? f)
                     (try
                       (let [f-env (apply f args)]
                         (tap> {:fn* f-env :env env})
                         (if (fn? f-env)
                           (let [result (f-env env)]
                             (tap> {:result result :env env})
                             result)
                           (throw (ex-info "Invoke function did not return a function"
                                           {:expr expr
                                            :invoke f
                                            :actual f-env}))))
                       (catch Exception e
                         (throw (ex-info "Failed to invoke fn"
                                         {:expr ?fn :args args}
                                         e))))
                     (throw (ex-info "Expression did not yeild a function"
                                     {:expr ?fn
                                      :actual f}))))
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

                 [{::tag :match ::match ?match ::expr ?expr} ?env]
                 (let [bindings (mge.m/interpret (mge.m/parse ?match) ?env {})
                       new-form (mge.m/unify (mge.m/parse ?expr) bindings)]
                   (tap> {:match ?match :expr ?expr :result new-form})
                   (interpret-template (parse new-form) ?env))

                 [{::tag :get ::path ?path} ?env]
                 (let [val (get-in ?env ?path)
                       result (interpret-template (parse val) ?env)]
                   (tap> {:get ?path :result val :final result})
                   result)

                 [{::tag :get-raw ::path ?path} ?env]
                 (let [result (get-in ?env ?path)]
                   (tap> {:get-raw ?path :result val :final result})
                   result)

                 [{::tag :hiccup
                   ::element (m/some ?tag)
                   ::props ?props
                   ::children ?children}
                  ?env]
                 (let [expr (if (first ?props)
                              [?tag (interpret-template ?props ?env)]
                              [?tag])]
                   (->> ?children
                        (map (partial interpret-template* ?env))
                        (into expr)))

                 [(!exprs ...) ?env]
                 (apply list (interpret-template !exprs ?env))

                 [[!exprs ...] ?env]
                 (transduce (map (partial interpret-template* ?env))
                            conj
                            !exprs)

                 [{& ?rest} ?env]
                 (reduce-kv (fn [m k v]
                              (assoc m k (interpret-template v ?env)))
                            {}
                            (into {} ?rest))

                 [?expr ?env]
                 ?expr)]
                 (when (instance? Exception result)
                   (throw result))
                 (tap> {:fn 'interpret-template :expr expr :env env :result result})
                 result))

(defn emitter
  [content]
  (let [template (parse content)]
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
 (emit {:a {:? :bar}} {:foo 4 :bar {:? "foo"}}) := {:a 4}
 (emit {:a {:?? :bar}} {:foo 4 :bar {:? "foo"}}) := {:a {:? "foo"}}
 (emit {:?* [:list]
        :%> "(fn [idx _] (mod idx 2))"
        :<= {0 ["li" {:? [:.]}]
             1 ["li.odd" {:? [:.]}]}}
       {:list ["first" "second" "third"]})
 := [["li" "first"] ["li.odd" "second"] ["li" "third"]]
 (emit {:?* [:list]
        :<= {:* {:? [:.]}}}
       {:list ["first" "second" "third"]})
 := ["first" "second" "third"]
 (emit {:?* [:list]
        :<= {:* {:? [:.]}}}
       {:list :not-a-list}) := []
 (emit '{:! (inc {:? [:var]})}
       {'inc (invoker inc)
        :var 1}) := 2
 (emit {:<> [:div]} {}) := [:div]
 (emit {:<> [:div {:prop "val"}]} {}) := [:div {:prop "val"}]
 (emit {:<> [:div "string"]} {}) := [:div "string"]
 (emit {:<> [:div {:? [:string]}]} {:string "string"}) := [:div "string"]
 (emit {:<> [:div {:? [:string]} {:? :partial}]}
       {:string "Hello,"
        :class "red"
        :partial {:<> ["span" {:class {:? :class}} "world"]}})
 := [:div "Hello," [:span {:class "red"} "world"]]
 (emit {:<> [:ul [[:li "1"] [:li "2"]]]} {}) := '[:ul ([:li "1"] [:li "2"])]
 (emit '{:$? {:key ?value} :$= ?value} {:key 1}) := 1
 (emit '{:$? {:key ?value} :$= [?value]} {:key 2}) := [2]
 (emit '{:$? {:key ?value} :$= {:? [?value]}} {:key :val :val 3}) := 3
 ; End
 )
