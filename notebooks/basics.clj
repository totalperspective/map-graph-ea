;; # The Basics
(ns basics
  {:nextjournal.clerk/toc true}
  (:require [nextjournal.clerk :as clerk]
            ; [viewers :as v]
            [missionary.core :as m]
            ; [map-graph-ea.core :as mge]
            [com.wsscode.pathom3.connect.indexes :as pci]
            [com.wsscode.pathom3.connect.operation :as pco]
            [com.wsscode.pathom3.interface.smart-map :as psm]))

;; The primary focus of map-graph-ea,
;; is to add reactivity to the [smart maps](https://pathom3.wsscode.com/docs/smart-maps/) provided by [Pathom3](https://pathom3.wsscode.com/).

;; ## Pathom3 Smart Maps

;; Panthom resovlers act on the keys of a map
(pco/defresolver full-name [{::keys [first-name last-name]}]
  {::full-name (str first-name " " last-name)})

;; The can be bundled together into an index,
;; this indicate which keys exist together.
(def indexes (pci/register full-name))

;; Given some initial data we can create a smart map.
(def person-data {::first-name "John" ::last-name "Lock"})
(def smart-map (psm/smart-map indexes person-data))

(::full-name smart-map) ; => "John Lock"

;; Modifying the smart map will have the desired effect on downstream keys
(-> smart-map
    (assoc ::last-name "Oliver")
    ::full-name) ; => "John Oliver", the full-name gets re-computed due to the change

;; This change is obviously immutable
(::full-name smart-map) ; => "John Lock"

;; ### Reactive Smart Maps

;; To make a smart map reactive we can simply use an atom
(defonce !smart-map (atom nil))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn init-map! []
  (reset! !smart-map smart-map))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn change-name! []
  (swap! !smart-map #(assoc % ::last-name "Oliver")))

^{::clerk/viewer '(fn [m]
                    [:div.text-center
                     (when m
                       [:div.mt-2 {:style {:font-size "2em"}} "Full Name: " m])
                     [:button.bg-blue-500.hover:bg-blue-700.text-white.font-bold.py-2.px-4.rounded
                      {:on-click (fn [e] (nextjournal.clerk.render/clerk-eval {:recompute? true} '(init-map!)))} "initialise map!"]
                     (when m
                       [:button.bg-blue-500.hover:bg-blue-700.text-white.font-bold.py-2.px-4.rounded
                        {:on-click (fn [e] (nextjournal.clerk.render/clerk-eval {:recompute? true} '(change-name!)))} "change map!"])])}
(::full-name @!smart-map)

;; ## Missionary

;; Missionary is a reactive dataflow programming toolkit.
;; We can implment the above example as a missionary flow.

(defonce !person-data (atom person-data))

;; We can create a flow to represent the changes to person-data
(def person-value (m/watch !person-data))

;; We can also create a flow to represent the indices we're interested in
^{::clerk/visibility {:result :hide}}
(def indexes-value (m/seed (repeat indexes)))

;; We can now compose our flows to represent a flow of smart maps
(def smart-person-value (m/latest psm/smart-map
                                  (m/signal indexes-value)
                                  (m/signal person-value)))

;; And again for the key we're interested in
(def full-name-value (m/signal (m/latest ::full-name smart-person-value)))

(defonce !full-name (atom nil))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn init-person! []
  (prn "init-person!")
  (prn (reset! !person-data person-data))
  (let [<full-name (m/signal (m/eduction (take 1) full-name-value))
        _ (prn <full-name)
        task (m/reduce (fn [_ n]
                         (println "init-person! " n)
                         (reset! !full-name n))
                       nil
                       <full-name)
        _ (prn task)
        dispose! (task #(prn ::success %)
                       #(prn ::crash %))]
    (prn dispose!)
    (prn (dispose!))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn change-person-name! []
  (swap! !person-data #(assoc % ::last-name "Oliver")))

^{::clerk/viewer '(fn [m]
                    [:div.text-center
                     (when m
                       [:div.mt-2 {:style {:font-size "2em"}} "Full Name: " m])
                     [:button.bg-blue-500.hover:bg-blue-700.text-white.font-bold.py-2.px-4.rounded
                      {:on-click (fn [e] (nextjournal.clerk.render/clerk-eval {:recompute? true} '(init-person!)))} "initialise map!"]
                     (when m
                       [:button.bg-blue-500.hover:bg-blue-700.text-white.font-bold.py-2.px-4.rounded
                        {:on-click (fn [e] (nextjournal.clerk.render/clerk-eval {:recompute? true} '(change-person-name!)))} "change map!"])])}
@!full-name

(def !input (atom 1))
(def main                                      ; this is a reactive computation, the println reacts to input changes
  (let [<x (m/signal (m/watch !input))         ; continuous signal reflecting atom state
        <y (m/signal (m/latest + <x <x))]      ; derived computation, diamond shape
    (m/reduce (fn [_ x] (prn x)) nil <y)))     ; discrete effect performed on successive values

(def dispose!
  (main
   #(prn ::success %)
   #(prn ::crash %)))                         ; prints 2
(swap! !input inc)                             ; prints 4
                                               ; Each change on the input propagates atomically through the graph.
                                               ; 3 is an inconsistent state and is therefore not computed.

(dispose!)                                     ; cleanup, deregisters the atom watch
