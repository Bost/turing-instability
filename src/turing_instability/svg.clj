(ns turing-instability.svg
  (:use
        [hiccup.core]
        [hiccup.page :only (include-css
                             ;include-js
                                        )]
        [ring.adapter.jetty]
        [compojure.core]
        [turing-instability.init]
  )
  (:require
    [compojure.route :as route]
  )
  )

(comment
(load "../turing_instability/svg")
)

(defn circle [x y]
  "usage: (tag-circle 20 30)"
  "<circle cx="20" cy="30" r="4" fill=\"blue\"></circle>"
  [:circle {:cx (str x) :cy (str y) :r "2" :fill "red"}]
  )

(defn webpage []
  ""
  (def n 20)
  (def y 20)
  (def y-length 4)
  (def x-width 20)
  (def x-offset 20)
  (def y-offset 20)
  (def scale-line-width 2)


 (html
   [:html
    [:head
     [:title "Turing instability"]
     (include-css stylesheet)
     ]
    [:body {:id "browser"}
     [:p {:class "ex" } "user.dir: " (System/getProperty "user.dir")]
     [:svg {:xmlns "http://www.w3.org/2000/svg" :version "1.1" }
;    (circle 20 40)
;    (circle 40 40)

      ; line with x-axis
      [:line {:x1 x-offset
              :y1 y-offset
              :x2 (+ x-offset (* x-width n))
              :y2 y-offset
              :style "stroke:rgb(0,0,0);stroke-width:2"} ]

    ; short lines to see the scale of the x-axis
    (for [i (range  0 (+ 1 n))]
      [:line {:x1 (+ x-offset (* i x-width))
              :y1 y-offset
              :x2 (+ x-offset (* i x-width))
              :y2 (+ y-offset y-length)
              :class "cls"
              }
       ]
      )
    ]]]; svg, body, html
   ))

(defroutes webroutes
           (GET webroute [] (webpage))
           (route/files "/"))

(defn webserver []
  (run-jetty (var webroutes) {:port port
                              :join? false  ; :join? - Block the caller: defaults to true
                              }))

;(webserver)
;(println (str "Webserver started on http://localhost:" port webroute))

