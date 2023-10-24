;; # UI
(ns ui
  (:require [map-graph-ea.component :as mge.c]))

(def layout
  '{:query [:page :components :auth :menu]
    :content {:type "default-layout"
              :props {:header {:! (component {:? [:components :header]})}
                      :menu {:! (component {:? [:components :menu]})}}
              :children [{:type {:? [:page :type]}
                          :content {:? [:page :content]}}]}})

(def header
  {:query  [:auth :page]
   :content {:type "header"
             :props {:title {:? [:page :name]}
                     :user {:id {:? [:auth :user :id]}
                            :name {:? [:auth :user :diplay-name]}}}}})

(def menu
  {:query [{:menu [{:items [:name :type :route]}]}]
   :content {:type "aside-menu"
             :children {:?* [:menu :items]
                        :<= {:* {:? []}}}}})

(def components
  {:header header
   :menu menu})

(def layouts
  {:default (mge.c/parse layout)})

(def env {:components components
          :page {:type "test"
                 :name "Test Page"
                 :content "test-content"}
          :auth {:user {:id 1 :name "Zaphod B"}}
          :menu {:items []}})

(try
  (let [page (:default layouts)]
    (page env))
  (catch Exception e
    (ex-data e)))
