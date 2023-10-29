(ns map-graph-ea.match
  (:require [meander.epsilon :as m]
            [hyperfiddle.rcf :refer [tests]]))

#_{:clj-kondo/ignore [:syntax :unused-binding :unresolved-var]}
(defn parse [expr]
  (m/rewrite expr
             [!xs ...]
             {:tag :vector
              :sequence [(m/cata !xs) ...]}

             (m/symbol _ (m/re #"^?.*") :as ?symbol)
             {:tag :logic-variable
              :symbol ?symbol}

             {& (m/seqable [!k !v] ...)}
             {:tag :map
              :entries [[!k (m/cata !v)] ...]}

             ?x
             {:tag :expr
              :expr ?x}))

(defn interpret [expr target env]
  (m/match [expr target env]

    [{:tag :logic-variable :symbol ?symbol} ?target ?env]
    (if (contains? ?env ?symbol)
      (if (= ?target (get ?env ?symbol))
        ?env
        :fail)
      (assoc ?env ?symbol ?target))

    ;; Ensure target is a vector
    [{:tag :vector :checked nil :sequence ?sequence} ?target ?env]
    (if (vector? ?target)
      (interpret {:tag :vector :checked true :sequence ?sequence} ?target ?env)
      :fail)

    [{:tag :vector :sequence []} _ ?env]
    ?env

    [{:tag :vector :sequence [?x]} ?target ?env]
    (interpret ?x (nth ?target 0) ?env)

    [{:tag :vector :checked ?checked :sequence [?x & ?rest]} ?target ?env]
    (interpret {:tag :vector :checked ?checked :sequence ?rest}
               (subvec ?target 1)
               (interpret ?x (nth ?target 0) ?env))

    [{:tag :map :checked nil :entries ?entries} ?target ?env]
    (if (map? ?target)
      (interpret {:tag :map :checked true :entries ?entries} ?target ?env)
      :fail)

    [{:tag :map :entries []} _ ?env]
    ?env

    [{:tag :map :entries [[?k ?v]]} ?target ?env]
    (interpret ?v (get ?target ?k) ?env)

    [{:tag :map :checked ?checked :entries [[?k ?v] & ?rest]} ?target ?env]
    (interpret {:tag :map :checked ?checked :entries ?rest}
               (dissoc ?target ?k)
               (interpret ?v (get ?target ?k) ?env))

    [{:tag :expr :expr _} ?env]
    ?env))

(defn unify [expr env]
  (m/match [expr env]
    [{:tag :logic-variable :symbol ?symbol} ?env]
    (get ?env ?symbol)

    [{:tag :vector :sequence []} _]
    []

    [{:tag :vector :sequence [& ?rest]} ?env]
    (mapv #(unify % ?env) ?rest)

    [{:tag :map :entries []} _]
    {}

    [{:tag :map :entries [[?k ?v]]} ?env]
    {?k (unify ?v ?env)}

    [{:tag :map :entries [[?k ?v] & ?rest]} ?env]
    (merge {?k (unify ?v ?env)}
           (unify {:tag :map :entries ?rest} ?env))

    [{:tag :expr :expr ?expr} _]
    ?expr))

(tests
 (parse '?x) := '{:tag :logic-variable
                  :symbol ?x}

 (parse '[?x ?y]) := '{:tag :vector
                       :sequence [{:tag :logic-variable
                                   :symbol ?x}
                                  {:tag :logic-variable
                                   :symbol ?y}]}

 (parse '{:x ?x}) := '{:tag :map
                       :entries [[:x {:tag :logic-variable
                                      :symbol ?x}]]}

 (interpret (parse '[?x ?y]) [1 2] {}) := '{?x 1 ?y 2}

 (interpret (parse '{:x ?x :y ?y}) {:x 1 :y 2} {}) := '{?x 1 ?y 2}

 (unify (parse '?x) '{?x 1}) := 1

 (unify (parse '[?x ?x]) '{?x 1}) := [1 1]

 (unify (parse '[?x ?y]) '{?x 1 ?y 2}) := [1 2]

 (unify (parse '{:x ?x :y ?y}) '{?x 1 ?y 2}) := {:x 1 :y 2}

 (unify (parse '{:x ?x :y [?y]}) '{?x 1 ?y 2}) := {:x 1 :y [2]}

 (unify (parse '{:x ?x :y [{:z ?y}]}) '{?x 1 ?y 2}) := {:x 1 :y [{:z 2}]}

 (unify (parse '[:path :to ?value]) '{?value :val}) := [:path :to :val]
 ;
 )
