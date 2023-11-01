;; # Components
(ns map-graph-ea.component
  (:require [com.wsscode.pathom3.connect.built-in.resolvers :as pbir]
            [com.wsscode.pathom3.connect.indexes :as pci]
            [com.wsscode.pathom3.interface.eql :as p.eql]
            [com.wsscode.pathom3.interface.smart-map :as psm]
            [hyperfiddle.rcf :as rcf :refer [tests]]
            [malli.core :as mc]
            [malli.error :as me]
            [malli.transform :as mt]
            [map-graph-ea.eql :refer [Query]]
            [map-graph-ea.template :as t :refer [Template]]
            [map-graph-ea.resolve :as r]
            [meander.epsilon :as m]
            [hasch.core :as h])
  (:import [java.lang Exception]))

;; Components are combine EQL queries and local resolvers to produce some content
(def Component
  (mc/schema [:map
              [:query Query]
              [:resolve {:optional true}
               [:map-of :keyword [:or
                                  [:cat :symbol any?]
                                  [:cat :symbol :keyword [:+ any?]]]]]
              [:content Template]]))

(defn resolvers
  [resolve-map]
  #_{:clj-kondo/ignore [:unresolved-symbol :unresolved-var :syntax]}
  (m/rewrite
   resolve-map

   {:to ?to :args [?resolver & ?args]}
   [?resolver ?to & ?args]

   {& (m/seqable [!to !args] ...)}
   [(m/cata {:to !to :args !args}) ...]))

(tests
 (resolvers {:foo ['bar :baz]}) := [['bar :foo :baz]]
 (resolvers {:foo ['bar :baz]
             :a ['b :c 1]}) := [['bar :foo :baz]
                                ['b :a :c 1]])

(def cache (atom {}))

(declare parse)

;; The parse take a component definition and returns a function.
;; The function, given an environment will run the compoent yeilding the content
(defn parse-impl
  [{:keys [query resolve content] :as t}]
  (let [component-fn (fn component [form]
                       (let [c-hash (h/edn-hash form)
                             cached-c (get @cache c-hash)]
                         (if cached-c
                           cached-c
                           (let [c (parse form)]
                             (swap! cache assoc c-hash c)
                             c))))
        indexes (->> resolve
                     resolvers
                     (mapv r/resolver)
                     (into [(pbir/constantly-resolver :portal.rpc/id nil)
                            (pbir/constantly-resolver 'component component-fn)])
                     pci/register)
        emit (t/emitter content)]
    (fn render [env]
      (tap> {:fn `render :template t :env env})
      (let [data (p.eql/process (pci/register [(pbir/global-data-resolver env)])
                                query)
            smart-map (psm/smart-map indexes data)]
        (tap> {:fn `render :query query :data data :map smart-map})
        (if (map? smart-map)
          (emit smart-map)
          (throw (ex-info "Query failed" {:data data :map smart-map})))))))

(defn valid-component? [spec]
  (mc/validate Component spec))

(defn parse
  [form]
  (tap> {:fn `parse
         :form form})
  (let [spec (mc/decode Component form mt/json-transformer)]
    (tap> {:fn `parse
           :spec spec})
    (if (valid-component? spec)
      (parse-impl spec)
      (let [error (mc/explain  Component spec)]
        (try
          (throw (ex-info "Invalid Component"
                          {:component form
                           :error (me/humanize error)
                           :detail (me/error-value error {::me/mask-valid-values '...})
                           :full error}))
          (catch Exception e
            (throw (ex-info "Unknown Error" {:error error} e))))))))
(tests
 "V node style"
 (def c (parse {:query ["some-value"]
                :resolve {:init-value ["x-form" "some-value" "#(* % 2)"]}
                :content {:type "my-input"
                          :props {:value {:? ["init-value"]}
                                  :label "Value:"}
                          :children []}}))
 (def data {:some-value 1})
 (c data) := {:type "my-input"
              :props {:value 2
                      :label "Value:"}
              :children []})

(tests
 "Hiccup style"
 (def c (parse {:query ["some-value"]
                :resolve {:init-value ["x-form" "some-value" "#(* % 2)"]}
                :content {:<> [:div [:my-input
                                     {:value {:? ["init-value"]}
                                      :label "Value:"}]]}}))
 (def data {:some-value 1})
 (c data) := [:div [:my-input {:value 2
                               :label "Value:"}]])
(comment
  ;; End.
  )
