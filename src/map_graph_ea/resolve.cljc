;; # Resolvers
(ns map-graph-ea.resolve
  (:require [hyperfiddle.rcf :as rcf :refer [tests ! %]]
            [com.wsscode.pathom3.connect.indexes :as pci]
            [com.wsscode.pathom3.connect.built-in.resolvers :as pbir]
            [com.wsscode.pathom3.interface.eql :as p.eql]
            [malli.core :as mc]
            [malli.transform :as mt]
            [malli.error :as me]
            [sci.core :as sci]))

;; ## The Registry
(def SchemaRegistry
  (atom {:schemas []
         :base [:or]}))

(defn reg-schema 
  [schema]
  (swap! SchemaRegistry
         (fn [reg]
           (let [{:keys [schemas base] :as new-reg} (update reg :schemas conj schema)]
             (assoc new-reg :schema (mc/schema (into base schemas)))))))

;; ## The Resolver
;; Resolvers are dispatched via a multi-method
(defmulti resolver-impl first)

;; This is the resolver it wraps the multimethod to coerce the incomming form,
;; based on the registered schemas.
(defn resolver
  [form]
  (let [Resolver (:schema @SchemaRegistry)
        spec (mc/decode Resolver form mt/json-transformer)]
    (if (mc/validate Resolver spec)
      (resolver-impl spec)
      (let [error (mc/explain  Resolver spec)
            message (me/humanize error)]
        (throw (ex-info "Invalid Resolver"
                        {:error message
                         :detail (me/error-value error {::me/mask-valid-values '...})}))))))

;; This helper function alows for the simple evaluation of a resolver
(defn eval-1
  [resolver data eql]
  (p.eql/process (pci/register [resolver]) data eql))

;; ## Built-in Resolvers
;; ### Constants
;; The `always` resolver allows a key to resolve to a constant value
(def Num
  (mc/schema [:or :int :double]))

(def Scalar
  (mc/schema [:or :string :boolean Num]))

(def Always
  (mc/schema [:tuple {:title "always"}
              [:and :symbol [:= 'always]] :keyword [:maybe Scalar]]))

(reg-schema Always)

(defmethod resolver-impl 'always
  [[_ to val]]
  (pbir/constantly-resolver to val))

(tests
 (-> (resolver ["always" "some/value" 1])
     (eval-1 {} [:some/value])
     :some/value) := 1)

;; ### Aliases
;; The `alias` resolver binds two keywords unidirectionally `:a` and `:b`,
;; so that `:a` resolves to `:b`.
(def Alias
  (mc/schema [:tuple {:title "alias"}
              [:and :symbol [:= 'alias]] :keyword :keyword]))

(defmethod resolver-impl 'alias
  [[_ to from]]
  (pbir/alias-resolver from to))

(reg-schema Alias)

(tests
  (-> (resolver ["alias" "some/value" "other/value"])
     (eval-1
      {:other/value 2}
      [:some/value])
     :some/value) := 2)

;; The `same-as` resolver is similar to `alias` but is bidirectional.
(def SameAs
  (mc/schema [:tuple {:title "same-as"} 
              [:and :symbol [:= 'same-as]] :keyword :keyword]))

(defmethod resolver-impl 'same-as
  [[_ x y]]
  (pbir/equivalence-resolver x y))

(reg-schema SameAs)

(tests
 (-> (resolver ["same-as" "some/value" "other/value"])
     (eval-1
      {:other/value 3}
      [:some/value])
     :some/value) := 3)

;; The `x-form` resolver transforms a value from one key,
;; to another,
;; via a single arity function.
(def XForm
  (mc/schema [:tuple {:title "x-form"}
              [:and :symbol [:= 'x-form]] :keyword :keyword :string]))

(defmethod resolver-impl 'x-form
  [[_ to from fn-str]]
  (let [fn (sci/eval-string fn-str)]
    (if (fn? fn)
      (pbir/single-attr-resolver from to fn)
      (throw (ex-info "Expression diud not evaluate to a function"
                      {:expression fn-str
                       :result fn})))))
(reg-schema XForm)

(tests
 (-> (resolver ["x-form" "some/twice" "some/value" "#(* % 2)"])
     (eval-1
      {:some/value 2}
      [:some/twice])
     :some/twice) := 4)

(comment
  ;; End
  )