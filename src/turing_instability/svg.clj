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

(defn get-from-vec [v f]
  (reduce f (vec v)))

(defn getval [v0 v1 f]
  (f (get-from-vec v0 f) (get-from-vec v1 f)))

(defn webpage [last-day f0 f1]
  "compute values for n days using functions f0, f1"
  (let [
        first-day 1
        list-of-days (range first-day (+ 1 last-day))
        vals-f0 (map #(f0 %) list-of-days)
        data-f0 (map vector list-of-days vals-f0)

        vals-f1 (map #(f1 %) list-of-days)
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
      [:div {:class "small" :style "color: red" } (str "s:" s "; f0:" 'j-jt-diff)]
      [:div {:class "small" :style "color: blue"} (str "p:" p "; f0:" 'r-rt-diff)]
      ;[:div {:class "small"} (str "vals-f0:" (vec vals-f0))]
      ;[:div {:class "small"} (str "vals-f1:" (vec vals-f1))]
      ;[:div {:class "small"} (str "min:" (getval vals-f0 vals-f1 min))]
      ;[:div {:class "small"} (str "max:" (getval vals-f0 vals-f1 max))]
      [:div {:class "small"} (str "j :" (vec (map #(j  %) list-of-days)))]
      [:div {:class "small"} (str "r :" (vec (map #(r  %) list-of-days)))]
      [:div {:class "small"} (str "jt:" (vec (map #(jt %) list-of-days)))]
      [:div {:class "small"} (str "rt:" (vec (map #(rt %) list-of-days)))]
      [:div {:class "small"} (str "j-jt-diff:" (vec (map #(j-jt-diff %) list-of-days)))]
      [:div {:class "small"} (str "r-rt-diff:" (vec (map #(r-rt-diff %) list-of-days)))]
      [:div {:class "small"} (str "j-jt-diff-n1:" (vec (map #(j-jt-diff-n1 %) list-of-days)))]
      [:div {:class "small"} (str "r-rt-diff-n1:" (vec (map #(r-rt-diff-n1 %) list-of-days)))]
      ;[:div {:class "small"} (str "day-1-j :" day-1-j)]
      ;[:div {:class "small"} (str "day-1-r :" day-1-r)]
      ;[:div {:class "small"} (str "day-1-jt :" day-1-jt)]
      ;[:div {:class "small"} (str "day-1-rt :" day-1-rt)]

      (emit-svg
        (-> (xy-plot :xmin first-day :xmax last-day,
                     :ymin (getval vals-f0 vals-f1 min)
                     :ymax (getval vals-f0 vals-f1 max)
                     :height 500 :width 1000
                     ;:label-points? true
                     )
          (add-points data-f0 :transpose-data? true :fill "red" :size 6)
          (add-points data-f1 :transpose-data? true :fill "blue")
          ))
      ]]; body, html
    )))

(defroutes webroutes
           (GET webroute [] (webpage
                              20     ; days
                              ;j-jt-diff r-rt-diff
                              ;j-jt-diff-n1 r-rt-diff-n1
                              jt rt
                              ;j r
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

