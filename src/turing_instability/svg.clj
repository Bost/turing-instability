(ns turing-instability.core
  (:use hiccup.core)
  (:use ring.adapter.jetty)
  (:use compojure.core)
  )

(defn mockup-2 []
 (html
  [:head [:title "Mini-Browser"]]
  [:body {:id "browser"}
   
   [:div {:id "header"} [:h2 "Mini-Browser"]]
   
   [:div {:id "content"} "Body"]
   
   [:div {:id "footer"}
    (str "Jedna plus dva rovna sa " (+ 1 3) )]
   ]
   ;[:circle {:cx "20" :cy "30" :fill "blue"}]
  
  ))


;(defroutes mockup-routes (GET "/m2" [] (mockup-2)))  
;
;  (defn mockup-server []
;      (run-jetty (var mockup-routes) {:port 8999
;                                      :join? false}))
;
;  (mockup-server)
