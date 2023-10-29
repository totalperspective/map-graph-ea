;; # Viewers
(ns viewers
  {:nextjournal.clerk/toc true}
  (:require [nextjournal.clerk :as clerk]))

;; This is a custom viewer for [Mermaid](https://mermaid-js.github.io/mermaid), a markdown-like syntax for creating diagrams from text. Note that this library isn't bundles with Clerk but we use a component based on [d3-require](https://github.com/d3/d3-require) to load it at runtime.
 #_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
 (def mermaid {:transform-fn clerk/mark-presented
              :render-fn '(fn [value]
                            (when value
                              [nextjournal.clerk.render/with-d3-require {:package ["mermaid@8.14/dist/mermaid.js"]}
                               (fn [mermaid]
                                 [:div {:ref (fn [el] (when el
                                                        (.render mermaid (str (gensym)) value #(set! (.-innerHTML el) %))))}])]))})

 #_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
 (def vue-vnode {:transform-fn clerk/mark-presented
                 :render-fn '(fn [value]
                               (when value
                                 (let [vue->hiccup (fn vue->hiccup [vnode]
                                                     (let [{:keys [type props children key]} vnode]
                                                       (->> children
                                                            (map vue->hiccup)
                                                            (into [(keyword type)
                                                                   (assoc props :key (or key (hash vnode)))]))))]
                                   (vue->hiccup value))))})
