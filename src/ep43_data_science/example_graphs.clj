(ns ep43-data-science.example-graphs
  (:require [ep43-data-science.graphs :as g]
            [ep43-data-science.abalone :as abalone]
            [redux.core :as redux]
            [kixi.stats.core :as kixi]
            [net.cgrand.xforms :as x]
            [thi.ng.geom.viz.core :as viz]))

;; Redefine this with your preferred method of viewing the graphs
(defn show [spec]
  (g/open spec)
  #_(g/open-with spec "firefox")
  #_(g/render-to-file "graph.svg"))

;; Histogram clearly showing the log-normal shape of the data. The area of 1
;; standard deviation from the median is given a darker color. Some jittering is
;; added to make the discrete ring counts more amenable to binning.
(let [[sd mean] (transduce (map (comp #(Math/log %) :rings))
                           (redux/juxt kixi/standard-deviation
                                       kixi/median)
                           abalone/data)]
  (-> {:data      abalone/data
       :x         (comp g/ln (g/jitter 0.5) :rings)
       :bin-count 28
       :stroke    (fn [[rings count]]
                    (if (<= (- mean sd) rings (+ mean sd))
                      "#3385FF"
                      "#77CCFF"))}
      g/histogram
      show))

;; Barcode plot of the height data. The two highest measurements are extreme
;; outliers, we'll assume they're mistakes and drop them. Different sexes are
;; represented with colors.
(-> {:data           (sequence (comp (x/sort-by :height) (x/drop-last 2)) abalone/data)
     :x              (comp (g/jitter 0.5) :height)
     :width          1000
     :height         150
     :stroke-width   "1px"
     :stroke         #(case (:sex %)
                        "M" "rgb(20,50,150)"
                        "F" "rgb(150,20,50)"
                        "I" "rgb(20,150,50)")
     :stroke-opacity 0.1}
    show)

(-> {:data abalone/data
     :x (comp (g/jitter 0.5) :rings)
     :y :shell-weight
     :x-axis-fn viz/log-axis
     :y-axis-fn viz/log-axis
     :grid-minor-x? true
     :grid-minor-y? true}
    g/scatter-plot
    show)
