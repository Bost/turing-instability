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

    [analemma.charts :only [emit-svg xy-plot add-points]]
    [analemma.svg]
    )
  (:require
    [compojure.route :as route]
    )
  )

(comment  ; use these commands on repl
         (load "../turing_instability/svg")
         (in-ns 'turing-instability.svg)
         )

(defn scale [v]
  "Scale given value v for the range which can be displayed on the graph"
  (floor (* 100 v))
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
(def analemma-data (map vector days scaled-vals))

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

      (emit-svg
        (-> (xy-plot :xmin 0 :xmax n,
                     :ymin min-val :ymax max-val
                     :height 500 :width 500)
          (add-points analemma-data)))
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

