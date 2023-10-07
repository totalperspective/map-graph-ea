(ns map-graph-ea.core
  (:require [missionary.core :as m]
            [com.wsscode.pathom3.connect.indexes :as pci]
            [com.wsscode.pathom3.interface.smart-map :as psm]
            [hyperfiddle.rcf :as rcf :refer [tests ! %]]))

(defn source
  [val]
  (let [!ref (atom val)
        <flow (m/watch !ref)]
    {:!ref !ref
     :<flow <flow}))

(defn >!
  [{:keys [!ref]} val]
  (if !ref
    (reset! !ref val)
    (throw (ex-info "Must have a !ref key" {}))))

(defn >!!
  [{:keys [!ref]} fn]
  (if !ref
    (swap! !ref fn)
    (throw (ex-info "Must have a !ref key" {}))))

(defn =<
  [{:keys [<flow]}]
  (if <flow
    <flow
    (throw (ex-info "Must have a <flow key" {}))))


(defn <<map
  [!!indexes !!data]
  (let [<indexes (=< !!indexes)
        <data (=< !!data)]
    {:<flow (m/ap (let [indexes (m/?< <indexes)
                        data (m/?< <data)]
                    (psm/smart-map indexes data)))}))

; prevent test execution during cljs hot code reload
#?(:cljs (defn ^:dev/before-load stop [] (hyperfiddle.rcf/enable! false)))
#?(:cljs (defn ^:dev/after-load start [] (hyperfiddle.rcf/enable!)))

(rcf/set-timeout! 100)

(tests
 (tests
  "sources"
  (def !!x (source 0))
  (def dispose ((m/reactor
                 (m/stream! (m/ap (! (inc (m/?< (=< !!x)))))))
                (fn [_] #_(prn ::done))
                #(do %)))
  % := 1
  (>!! !!x inc)
  (>!! !!x inc)
  % := 2
  % := 3
  (dispose))
 (tests
  "maps"
  (def indexes (pci/register []))
  (def m (psm/smart-map indexes {:x 1}))
  (:x m) := 1
  (def !!indexes (source indexes))
  (def !!data (source {:x 1}))
  (def !!m (<<map !!indexes !!data))
  (def dispose ((m/reactor
                 (m/stream! (m/ap (! (:x (m/?< (=< !!m)))))))
                (fn [_] #_(prn ::done))
                #(prn ::maps.crash %)))
  % := 1
  (dispose)))