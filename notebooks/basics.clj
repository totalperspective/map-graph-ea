;; # The Basics
(ns basics
  {:nextjournal.clerk/toc true}
  (:require [nextjournal.clerk :as clerk]
            [viewers :as v]
            [map-graph-ea.core :as mge]
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

;; ## Reactive Smart Maps

;; To make a smart map reactive we can simply use an atom
(defonce !smart-map (atom nil))

(defn init-map! []
  (reset! !smart-map smart-map))

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
