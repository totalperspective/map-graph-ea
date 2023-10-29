;; # UI
(ns ui
  (:require [map-graph-ea.component :as mge.c]
            ))

(def layout
  '{:query [:page :components :auth :menu]
    :content {:<> [:div
                   {:<> [:div#header
                         {:! (component {:? [:components :header]})}]}
                   {:<> [:div#menu {:! (component {:? [:components :menu]})}]}
                   {:<> [:compoenent
                         {:is {:? [:page :type]}}
                         {:? [:page :content]}]}]}})

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
  {:header header
   :menu menu})

(def layouts
  (when layout-valid?
    {:default (mge.c/parse layout)}))

(def env {:components components
          :page {:type "div"
                 :name "Test Page"
                 :content "test-content"}
          :auth {:user {:id 1 :name "Zaphod B"}}
          :menu {:items []}})

^:nextjournal.clerk/auto-expand-results?
(def page (try
            (when layouts
              (let [page (:default layouts)]
                (page env)))
            (catch Exception e
              (ex-data e))))
