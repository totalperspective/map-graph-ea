(ns map-graph-ea.njs
  (:require [map-graph-ea.component :as mge.c]))

(def !env (atom {:components {}}))

(defn ^js render
  [layout env]
  (let [c {:query (into [] (keys env))
           :content {:! ('component {:?? [:components layout]})}}]
    (c (merge @!env env))))

(defn ^js addComponent
  [name spec]
  (let [c-name (keyword name)
        c-spec (js->clj spec)]
    (if (mge.c/valid-component? c-spec)
      (do
        (swap! !env #(assoc-in % [:components c-name] c-spec))
        nil)
      (mge.c/parse c-spec))))
