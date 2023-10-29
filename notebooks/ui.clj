;; # UI
(ns ui
  (:require [map-graph-ea.component :as mge.c]
            ))

(def layout
  '{:query [:page :components :auth :menu]
    :content {:<> [:div
                   [:div#header
                    {:! (component {:? [:components :page :header]})}]
                   [:div#menu {:! (component {:? [:components :page :menu]})}]
                   [:compoenent
                    {:is {:? [:page :type]}}
                    {:? [:page :content]}]]}})

(def layout-valid? (mge.c/valid-component? layout))

(def header
  {:query  [:auth :page]
   :content {:type "header"
             :props {:title {:? [:page :name]}
                     :user {:id {:? [:auth :user :id]}
                            :name {:? [:auth :user :diplay-name]}}}}})

(mge.c/valid-component? header)

(def menu
  {:query [{:menu [{:items [:name :type :route]}]}]
   :content {:type "aside-menu"
             :children {:?* [:menu :items]
                        :<= {:* {:? []}}}}})

(mge.c/valid-component? menu)

(def components
  {:layouts {:default (mge.c/parse layout)}
   :page {:header header
          :menu menu}})

(def env {:components components
          :page {:type "div"
                 :name "Test Page"
                 :content "test-content"}
          :layout "default"
          :auth {:user {:id 1 :name "Zaphod B"}}
          :menu {:items []}})

(def page '{:query [:page :components :auth :menu :layout]
            :content {:$? {:layout ?layout}
                      :$= {:! (component {:? [:components :layouts ?layout]})}}})

^:nextjournal.clerk/auto-expand-results?
(when (mge.c/valid-component? page)
  ((mge.c/parse page)  env))
