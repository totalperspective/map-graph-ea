;; # UI
(ns ui
  (:require [map-graph-ea.component :as mge.c]
            ))

(def layout
  '{:query [:page :components :auth :menu]
    :content {:<> [:div
                   [:div#header
                    {:! (component {:?? [:components :page :header]})}]
                   [:div#menu {:! (component {:?? [:components :page :menu]})}]
                   [:component {:is {:? [:page :type]}}]
                   ]}})

(when (not (mge.c/valid-component? layout))
  (try
    (mge.c/parse layout)
    (catch Exception e
      e)))

(def header
  {:query  [:auth :page]
   :content {:type "header"
             :props {:title {:? [:page :name]}
                     :user {:id {:? [:auth :user :id]}
                            :name {:? [:auth :user :diplay-name]}}}}})

(when (not (mge.c/valid-component? header))
  (try
    (mge.c/parse header)
    (catch Exception e
      e)))

(def menu
  {:query [{:menu [{:items [:name :type :route]}]}]
   :content {:<> [:ul.aside-menu
                  {:?* [:menu :items]
                   :<= {:* ["li" ["a" {:href {:? :href}} {:? :label}]]}}]}})

(when (not (mge.c/valid-component? menu))
  (try
    (mge.c/parse menu)
    (catch Exception e
      e)))

(def components
  {:layouts {:default layout}
   :page {:header header
          :menu menu}})

(def env {:components components
          :page {:type "div"
                 :name "Test Page"
                 :content "test-content"}
          :layout "default"
          :auth {:user {:id 1 :name "Zaphod B"}}
          :menu {:items [{:href "/" :label "Home"}]}})

(def page '{:query [:page :components :auth :menu :layout]
            :content {:$? {:layout ?layout}
                      :$= {:! (component {:?? [:components :layouts ?layout]})}}})

^:nextjournal.clerk/auto-expand-results?
(when (mge.c/valid-component? page)
  (try
    (let [c (mge.c/parse page)]
      (c env))
    (catch Exception e
      e)))
