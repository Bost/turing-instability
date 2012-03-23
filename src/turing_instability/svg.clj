(ns turing-instability.svg
  (:use
    [clojure.math.numeric-tower]
    [hiccup.core]
    [hiccup.page :only (include-css
                         ;include-js
                         )]
    [ring.adapter.jetty]
    [compojure.core]
    [turing-instability.init]
    [turing-instability.relfuncs]
    )
  (:require
    [compojure.route :as route]
    )
  )

(comment  ; use these commands on repl
         (in-ns 'turing-instability.svg)
         (load "../turing_instability/svg")
         )

(defn scale [v]
  "Scale given value v for the range which can be displayed on the graph"
  (floor (* 100 v))
  )

(defn circle [x y]
  "Prints svg tag: <circle cx="20" cy="30" r="4" fill=\"blue\"></circle>"
  (html
    [:circle {:id (str "id" x) :cx (str x) :cy (str y) :r "6" :fill "red"}]
    [:text {:id (str "popup" x) :x (str (int (- x 20))) :y (str (int (+ y 20))) :font-size "12" :fill "black" :visibility "hidden"} (str "x:" x " y:" y)
     [:set {:attributeName "visibility" :from "hidden" :to "visible" :begin (str "id" x ".mouseover") :end (str "id" x ".mouseout")}]
     ]
    )
  )

(def n 20)
(def y 20)
(def y-length 4)
(def x-width 20)
(def x-offset 20)
(def y-offset 20)
(def scale-line-width 2)

;first-n-vals [f n]
(def days (range 0 (+ 1 n)))
(def scaled-vals (map #(scale (j-n1 (+ 1 %))) days))
(def max-val (reduce max (vec scaled-vals)))
(def min-val (reduce min (vec scaled-vals)))
(def abs-min-val (abs min-val))

(defn display-scale-for-day [day-i]
  (html
    [:line {:x1 (+ x-offset (* day-i x-width))
            :y1 (+ y-offset (- y-length) (abs min-val))
            :x2 (+ x-offset (* day-i x-width))
            :y2 (+ y-offset y-length (abs min-val))
            :class "cls"
            }]))

(defn display-value-for-day [day-i value-day-i]
  ;(println "i:" i "; yi:" yi "; abs-min-val:" abs-min-val)
  (circle (+ x-offset (* day-i x-width))
          (int (+ y-offset
             (+ value-day-i
                ;(abs value-day-i)
                abs-min-val
                )))
          ;(+ y-offset 0)
          )
  )

(defn webpage []
  ""
  (html
    [:html
     [:head
      [:title "Turing instability"]
      (include-css stylesheet)
      ]
     [:body {:id "browser"}
      ;[:p {:class "ex" } "user.dir: " (System/getProperty "user.dir")]
      ;(map #(html [:div {:class "small"} "val: " %]) (vec scaled-vals))
      ;[:div {:class "small"} (str "delta: " (+ max-val (abs min-val)))]

      [:svg {:xmlns "http://www.w3.org/2000/svg" :version "1.1"
             :style "border: 1px; border-color: black; border-style: solid;"
             }

       ; display the x-axis
       [:line {:x1 x-offset
               :y1 (+ y-offset (abs min-val))
               :x2 (+ x-offset (* x-width n))
               :y2 (+ y-offset (abs min-val))
               :style "stroke:rgb(0,0,0);stroke-width:2"} ]

       ; short lines to see the scale of the x-axis
       (map display-scale-for-day days)

       (map #(display-value-for-day (int %)                      ; day-i
                                    (int (scale (j-n1 (+ 1 %)))) ; value-day-i
                                    )
            days)
       ]; svg
      ]]; body, html
    ))

(defroutes webroutes
           (GET webroute [] (webpage))
           (route/files "/"))

(defn websrv []
  "Starts the web server"
  (run-jetty (var webroutes) {:port port
                              :join? false  ; :join? - Block the caller: defaults to true
                              }))

(websrv)
; TODO use clojure.contrib.singleton for starting websrv
;(println (str "Web server started on http://localhost:" port webroute))

