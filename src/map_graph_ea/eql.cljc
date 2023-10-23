(ns map-graph-ea.eql
  (:require [hyperfiddle.rcf :refer [tests]]
            [malli.core :as mc]
            [malli.error :as me]
            [malli.registry :as mr]
            [malli.transform :as mt])
  (:import [java.lang Exception]))

(defn ->list
  [x]
  ;; (prn x)
  (apply list x))

(def spec
  {::property :keyword
   ::special-property [:and :symbol [:= '*]]
   ::ident-value [:or :string :int :boolean :keyword]
   ::ident [:tuple ::property ::ident-value]
   ::join-key [:orn [:prop ::property] [:ident ::ident] [:param-exp ::join-key-param-expr]]
   ::join [:map-of ::join-key ::join-query] ;; :min-count 1 :conform-keys true
   ::union [:map-of ::property ::query]
   ::recursion-depth [:and :int :pos-int]
   ::recursion [:orn [:depth ::recursion-depth] [:unbounded [:and :symbol [:= '...]]]]
   ::join-query [:orn [:query ::query] [:union ::union] [:recursion ::recursion]]
   ::param-key [:orn
                [:prop ::property]
                [:number :int]]
   ::params [:map-of ::param-key :any]
   ::param-expr-key [:orn
                     [:prop ::property]
                     [:join ::join]
                     [:iden ::ident]]
   ::param-expr [:and
                 [:catn [:expr ::param-expr-key] [:params [:? ::params]]]
                 [list? {:decode {:json ->list}}]]
   ::join-key-param-key [:orn
                         [:prop ::property]
                         [:ident ::ident]]
   ::join-key-param-expr [:and
                          [:catn [:expr ::join-key-param-key] [:params [:? ::params]]]
                          [list? {:decode {:json ->list}}]]
   ::mutation-key :symbol
   ::mutation-expr [:and
                    [:catn [:mutate-key ::mutation-key] [:params [:? ::params]]]
                    [list? {:decode {:json ->list}}]]
   ::mutation-join [:schema [:map-of ::mutation-expr ::query]] ;; :min-count 1 :conform-keys true
   ::mutation [:orn
               [:mutation [:schema [:ref ::mutation-expr]]]
               [:mutation-join [:schema [:ref ::mutation-join]]]]
   ::mutation-query [:catn [:mutation [:* ::mutation]]]
   ::query-expr [:orn
                 [:special ::special-property]
                 [:prop ::property]
                 [:join [:schema [:ref ::join]]]
                 [:ident ::ident]
                 [:param-exp [:schema [:ref ::param-expr]]]]
   ::query [:catn [:query-expr [:* ::query-expr]]]})

(def registry
  (merge
   (mr/schemas mc/default-registry)
   {:neg-int (mc/-simple-schema {:type :neg-int, :pred neg-int?})
    :pos-int (mc/-simple-schema {:type :pos-int, :pred pos-int?})}
   spec))

(defn schema
  [spec]
  (mc/schema spec {:registry registry}))

(def Query (schema ::query))

(def Mutation (schema ::mutation-query))

(defn valid-query?
  [query]
  (mc/validate Query query))

(defn valid-mutation?
  [query]
  (mc/validate Mutation query))

^{:nextjournal.clerk/auto-expand-results? true
  :nextjournal.clerk/no-cache true}
(tests
 "specs"
 (def errors
   (->> spec
        keys
        (map (fn [key]
               (try
                 (schema key)
                 nil
                 (catch Exception e
                   [key (ex-data e)]))))
        (into {})))
 (empty? errors) := true
 errors)

(defn parse-query
  [form]
  (let [query (mc/decode Query form mt/json-transformer)]
    (if (valid-query? query)
      query
      (let [error (mc/explain Query query)
            message (me/humanize error)]
        (throw (ex-info "Invalid Query"
                        {:error message
                         :detail (me/error-value error {::me/mask-valid-values '...})
                         :full error}))))))
(defn parse-mutation
  [form]
  (let [mutation (mc/decode Mutation form mt/json-transformer)]
    (if (valid-mutation? mutation)
      mutation
      (let [error (mc/explain Mutation mutation)
            message (me/humanize error)]
        (throw (ex-info "Invalid Mutation"
                        {:error message
                         :detail (me/error-value error {::me/mask-valid-values '...})
                         :full error}))))))

^{:nextjournal.clerk/auto-expand-results? true
  :nextjournal.clerk/no-cache true}
(tests
 "parsing queries"
 (def query-forms '#{; Empty
                     []
               ; Properties
                     [:album/name :album/year]
               ; All Props
                     [*]
               ; Joins
                     [{:favorite-albums
                       [:album/name :album/year]}]
                     [{:favorite-albums
                       [:album/name :album/year
                        {:album/tracks
                         [:track/name
                          :track/duration]}]}]
               ; Idents
                     [[:customer/id 123]]
                     [{[:customer/id 123]
                       [:customer/name :customer/email]}]
               ; Params
                     [(:foo {:with "params"})]
                     [([:ident "value"] {:with "param"})]
                     [{(:join-key {:with "params"})
                       [:sub-query]}]
                     [{([:ident "value"] {:with "params"})
                       [:sub-query]}]
                     [({:join-key
                        [:sub-query]}
                       {:with "params"})]
               ; Recursion
                     [:entry/name {:entry/folders ...}]
                     [:entry/name {:entry/folders 3}]
               ; Union
                     [{:chat/entries
                       {:message/id [:message/id :message/text :chat.entry/timestamp]
                        :audio/id   [:audio/id :audio/url :audio/duration :chat.entry/timestamp]
                        :photo/id   [:photo/id :photo/url :photo/width :photo/height :chat.entry/timestamp]}}]})
 (->> query-forms
      (map (fn [form]
             (try
               (parse-query form)
               (catch Exception e
                 (or (ex-data e)
                     (str e))))))
      (into #{}))
 := query-forms)

^{:nextjournal.clerk/auto-expand-results? true
  :nextjournal.clerk/no-cache true}
(tests
 "parsing mutations"

 (def mutation-forms '#{;Empty
                        []
                        ; Mutation
                        [(call.some/operation {:data "input"})]
                        ; Mutation Join
                        [{(call.some/operation {:data "input"})
                          [:response :key-a :key-b]}]})
 (->> mutation-forms
      (map (fn [form]
             (try
               (parse-mutation form)
               (catch Exception e
                 (or (ex-data e)
                     (str e))))))
      (into #{}))
 := mutation-forms)

^{:nextjournal.clerk/auto-expand-results? true
  :nextjournal.clerk/no-cache true}
(tests
 "parsing json queries"
 (def q->js
   (mc/encoder Query mt/json-transformer))
 (->> query-forms
      (map q->js)
      (map (fn [form]
             (try
               (parse-query form)
               (catch Exception e
                 (or (ex-data e)
                     (str e))))))
      (into #{}))
 := query-forms)

^{:nextjournal.clerk/auto-expand-results? true
  :nextjournal.clerk/no-cache true}
(tests
 "parsing json mutations"
 (def m->js
   (mc/encoder Mutation mt/json-transformer))
 (->> mutation-forms
      (map m->js)
      (map (fn [form]
             (try
               (parse-mutation form)
               (catch Exception e
                 (or (ex-data e)
                     (str e))))))
      (into #{}))
 := mutation-forms)

(comment
  ;; End
  )
