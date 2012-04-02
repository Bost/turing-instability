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

(defn webpage [last-day f0 f1]
  "compute values for n days using functions f0, f1"
  (let [
        first-day 1
        list-of-days (range first-day (+ 1 last-day))
        ;vals-f0 (map #(scale (f0 (+ 1 %))) list-of-days)
        vals-f0 (map #(f0 (+ 1 %)) list-of-days)
        data-f0 (map vector list-of-days vals-f0)

        ;vals-f1 (map #(scale (f1 (+ 1 %))) list-of-days)
        vals-f1 (map #(f1 (+ 1 %)) list-of-days)
        data-f1 (map vector list-of-days vals-f1)
        ]
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
      [:div {:class "small" :style "color: red" } (str "s:" s "; f0:" 'j-jt-diff-n1)]
      [:div {:class "small" :style "color: blue"} (str "p:" p "; f0:" 'r-rt-diff-n1)]

      (emit-svg
        (-> (xy-plot :xmin first-day :xmax last-day,
                     :ymin (reduce min (vec vals-f1))
                     :ymax (reduce max (vec vals-f1))
                     :height 500 :width 1000
                     ;:label-points? true
                     )
          (add-points data-f0 :transpose-data? true :fill (rgb 255 0 0))  ; red
          (add-points data-f1 :transpose-data? true :fill (rgb 0 0 255))  ; blue
          ))
      ]]; body, html
    )))

(defroutes webroutes
           (GET webroute [] (webpage
                              20     ; days
                              j-jt-diff-n1
                              r-rt-diff-n1
                              )
                )
           (route/files "/"))

(defn websrv []
  "Starts the web server"
  (run-jetty (var webroutes) {:port port
                              :join? false  ; :join? - Block the caller: defaults to true
                              }))

;(websrv)
; TODO use clojure.contrib.singleton for starting websrv
;(println (str "Web server started on http://localhost:" port webroute))

