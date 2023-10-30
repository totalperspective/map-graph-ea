(ns map-graph-ea.match
  (:require [meander.epsilon :as m]
            [hyperfiddle.rcf :refer [tests]]))

#_{:clj-kondo/ignore [:syntax :unused-binding :unresolved-var]}
(defn parse [expr]
  (tap> {:fn ::parse :expr expr})
  (m/rewrite expr
             [!xs ...]
             {:tag :vector
              :sequence [(m/cata !xs) ...]}

             (!xs ...)
             {:tag :list
              :sequence [(m/cata !xs) ...]}

             (m/symbol _ (m/re #"^\?.*") :as ?symbol)
             {:tag :logic-variable
              :symbol ?symbol}

             {& (m/seqable [!k !v] ...)}
             {:tag :map
              :entries [[!k (m/cata !v)] ...]}

             ?x
             {:tag :expr
              :expr ?x}))

(def sequences
  {:vector {:pred vector?
            :rest #(subvec % 1)}
   :list {:pred list?
          :rest rest}})

#_{:clj-kondo/ignore [:syntax :unused-binding :unresolved-var]}
(defn interpret [expr target env]
  (tap> {::interpret [expr target env]})
  (m/match [expr target env]

    [{:tag :logic-variable :symbol ?symbol} ?target ?env]
    (if (contains? ?env ?symbol)
      (if (= ?target (get ?env ?symbol))
        ?env
        :fail)
      (assoc ?env ?symbol ?target))

    ;; Ensure target is the correct kind of sequence
    [{:tag ?type :checked nil :sequence (m/some ?sequence)} ?target ?env]
    (let [{:keys [pred]} (sequences ?type)]
      (if (and pred (pred ?target))
        (interpret {:tag ?type :checked true :sequence ?sequence} ?target ?env)
        :fail))

    [{:sequence []} _ ?env]
    ?env

    [{:sequence [?x]} ?target ?env]
    (interpret ?x (nth ?target 0) ?env)

    [{:tag ?type :checked ?checked :sequence [?x & ?rest]} ?target ?env]
    (let [{:keys [rest]} (sequences ?type)]
      (if rest
        (interpret {:tag ?type :checked ?checked :sequence ?rest}
                   (rest ?target)
                   (interpret ?x (nth ?target 0) ?env))
        :fail))

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
  (tap> {::unify [expr env]})
  (m/match [expr env]
    [{:tag :logic-variable :symbol ?symbol} ?env]
    (get ?env ?symbol)

    [{:tag :vector :sequence []} _]
    []

    [{:tag :vector :sequence [& ?rest]} ?env]
    (mapv #(unify % ?env) ?rest)

    [{:tag :list :sequence []} _]
    ()

    [{:tag :list :sequence [& ?rest]} ?env]
    (apply list (map #(unify % ?env) ?rest))

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
 (parse 'x) := '{:tag :expr
                 :expr x}

 (parse '(?x ?y)) := '{:tag :list
                       :sequence [{:tag :logic-variable
                                   :symbol ?x}
                                  {:tag :logic-variable
                                   :symbol ?y}]}

 (parse '[?x ?y]) := '{:tag :vector
                       :sequence [{:tag :logic-variable
                                   :symbol ?x}
                                  {:tag :logic-variable
                                   :symbol ?y}]}

 (parse '{:x ?x}) := '{:tag :map
                       :entries [[:x {:tag :logic-variable
                                      :symbol ?x}]]}

 (interpret (parse '[?x]) [1] {}) := '{?x 1}

 (interpret (parse '[?x ?y]) [1 2] {}) := '{?x 1 ?y 2}

 (interpret (parse '(?x)) '(1) {}) := '{?x 1}

 (interpret (parse '(?x ?y)) '(1 2) {}) := '{?x 1 ?y 2}

 (interpret (parse '{:x ?x :y ?y}) {:x 1 :y 2} {}) := '{?x 1 ?y 2}

 (unify (parse '?x) '{?x 1}) := 1

 (unify (parse '[?x]) '{?x 1}) := [1]

 (unify (parse '[?x ?x]) '{?x 1}) := [1 1]

 (unify (parse '[?x ?y]) '{?x 1 ?y 2}) := [1 2]

 (unify (parse '(?x)) '{?x 1}) := '(1)

 (unify (parse '{:x ?x}) '{?x 1}) := {:x 1}

 (unify (parse '{:x ?x :y ?y}) '{?x 1 ?y 2}) := {:x 1 :y 2}

 (unify (parse '{:x ?x :y [?y]}) '{?x 1 ?y 2}) := {:x 1 :y [2]}

 (unify (parse '{:x ?x :y [{:z ?y}]}) '{?x 1 ?y 2}) := {:x 1 :y [{:z 2}]}

 (unify (parse '[:path :to ?value]) '{?value :val}) := [:path :to :val]

 (unify (parse '{:! (component {:? [:components :layouts ?layout]})})
        '{?layout :default})
 := '{:! (component {:? [:components :layouts :default]})})
 ;
