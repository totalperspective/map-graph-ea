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
            [meander.epsilon :as m]))

;; Components are combine EQL queries and local resolvers to produce some content
(def Component
  (mc/schema [:map
              [:query Query]
              [:resolve [:map-of :keyword [:or
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

;; The parse take a component definition and returns a function.
;; The function, given an environment will run the compoent yeilding the content
(defn parse-impl
  [{:keys [query resolve content]}]
  (let [indexes (->> resolve
                     resolvers
                     (mapv r/resolver)
                     pci/register)]
    (fn [env]
      (let [data (p.eql/process env query)
            smart-map (psm/smart-map indexes data)]
        (t/emit content smart-map)))))

(defn parse
  [form]
  (let [spec (mc/decode Component form mt/json-transformer)]
    (if (mc/validate Component spec)
      (parse-impl spec)
      (let [error (mc/explain  Component spec)
            message (me/humanize error)]
        (throw (ex-info "Invalid Component"
                        {:error message
                         :detail (me/error-value error {::me/mask-valid-values '...})}))))))

(tests
 (def c (parse {:query ["some-value"]
                :resolve {:init-value ["x-form" "some-value" "#(* % 2)"]}
                :content {:type "my-input"
                          :props {:value {:? ["init-value"]}
                                  :label "Value:"}
                          :children []}}))
 (def indexes (pci/register (pbir/global-data-resolver {:some-value 1})))
 (c indexes) := {:type "my-input"
                 :props {:value 2
                         :label "Value:"}
                 :children []})

(comment
  ;; End
  )
