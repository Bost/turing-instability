(ns turing-instability.svg
  (:use
        hiccup.core
        ring.adapter.jetty
        compojure.core
  ))

(comment
(load "../turing_instability/svg")
)

(defn tag-circle [x y]
  "usage: (tag-circle 20 30)"
  "<circle cx="20" cy="30" r="4" fill=\"blue\"></circle>"
  [:circle {:cx (str x) :cy (str y) :r "10" :fill "red"}]
  )



(defn tst2 []
  (str "tst2 text4"))

(comment
  ; just an example how to include css and js files
(ns labrepl.layout
  (:use [hiccup.core :only (html)]
        [hiccup.page-helpers :only (include-css include-js link-to)]))

(def default-stylesheets
  ["/stylesheets/shCore.css"
   "/stylesheets/shThemeDefault.css"
   "/stylesheets/application.css"])

(def default-javascripts
  ["/javascripts/jquery.js"
   "/javascripts/application.js"
   "/javascripts/shCore.js"
   "/javascripts/shBrushClojure.js"])

(defn home [body]
  (html
    [:head
     [:title "Clojure Labs"]
     (apply include-css default-stylesheets)
     (apply include-js default-javascripts)]
    [:body [:div {:id "header"} [:h2.logo "Clojure Labs"]]
     [:div {:id "content"} body]
     [:div {:id "footer"} "Copyright All Rights Reserved"]]))

)

(defn webpage []
 (html
   [:html
  [:head [:title "Turing instability"]]
  [:body {:id "browser"}
   [:div {:id "header"} [:h2 "Mini-Browser"]]
   [:div {:id "content"} "Body"]
   [:div {:id "footer"}
    (str "some evaluation: " (+ 1 4) )]
   [:div {:id "footer"}
    ]

   [:svg {:xmlns "http://www.w3.org/2000/svg" :version "1.1" }
    (tag-circle 20 40)
    ;[:circle {:cx "20" :cy "30" :r "10" :fill "black"}]
    ;[:circle {:cx "100" :cy "50" :r "40" :stroke "black" :stroke-width "2" :fill "red"} ]

    ]
   ]
    ]
   ))

(defroutes webroutes (GET "/webpage" [] (webpage)))

(defn webserver []
  (run-jetty (var webroutes) {:port 8080
                              :join? false  ; :join? - Block the caller: defaults to true
                              }))

;(webserver)
;(println "webserver loaded")

