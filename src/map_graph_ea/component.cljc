;; # Components
(ns map-graph-ea.component
  (:require [clojure.walk :refer [prewalk]]
            [com.wsscode.pathom3.connect.indexes :as pci]
            [com.wsscode.pathom3.interface.eql :as p.eql]
            [com.wsscode.pathom3.interface.smart-map :as psm]
            [com.wsscode.pathom3.connect.built-in.resolvers :as pbir]
            [hyperfiddle.rcf :as rcf :refer [tests]]
            [malli.core :as mc]
            [malli.error :as me]
            [malli.transform :as mt]
            [map-graph-ea.resolve :as r]
            [meander.epsilon :as m]))

;; Components are combine EQL queries and local resolvers to produce some content
(def Component
  (mc/schema [:map
              [:query [:vector any?]]
              [:resolve [:map-of :keyword [:cat :symbol [:+ any?]]]]
              [:content any?]]))

#_{:clj-kondo/ignore [:unresolved-symbol]}
(defn resolvers
  [resolve-map]
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

(defn emit
  [content ctx]
  (let [expand (fn [node]
                 (if (and (vector? node) (= (count node) 2) (= :? (first node)))
                   (let [[_ key] node]
                     (emit (if (vector? key)
                             (get-in ctx key)
                             (get ctx key))
                           ctx))
                   node))]
    (prewalk expand content)))

(tests
 (emit 1 {}) := 1
 (emit [:? :foo] {:foo 2}) := 2
 (emit [1 2] {}) := [1 2]
 (emit {:a 1} {}) := {:a 1}
 (emit {:a [:? :foo] :b [:? :bar]} {:foo 3}) := {:a 3 :b nil}
 (emit {:a [:? :bar]} {:foo 4 :bar [:? :foo]}) := {:a 4})

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
        (emit content smart-map)))))

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
 (def c (parse {:query [:some-value]
                :resolve {:init-value ['alias :some-value]}
                :content {:type :my-input
                          :props {:value [:? :init-value]
                                  :label "Input:"}
                          :children []}}))
 (def indexes (pci/register (pbir/global-data-resolver {:some-value 1})))
 (c indexes) := {:type :my-input
                 :props {:value 1
                         :label "Input:"}
                 :children []})
