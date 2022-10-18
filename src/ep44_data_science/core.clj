(ns ep44-data-science.core
  (:require [ep43-data-science.abalone :as abalone]
            [ep43-data-science.graphs :as g]
            [kixi.stats.core :as kixi]
            [kixi.stats.distribution :as kixi.dist]
            [kixi.stats.math :as m]
            [net.cgrand.xforms :as x]
            [redux.core :as redux]))

(into {}
      (comp cat (x/by-key key val (x/reduce kixi/summary)))
      abalone/data)
;; =>
;; {:rings          {:min 1.0, :q1 8.0, :median 9.266666666666667, :q3 11.0, :max 29.0, :iqr 3.0},
;;  :sex            {:min nil, :q1 nil, :median nil, :q3 nil, :max nil, :iqr nil},
;;  :shell-weight   {:min 0.3,:q1 26.01370731707317,:median 46.70705294705295,:q3 65.77664576802508,:max 201.0,:iqr 39.76293845095191},
;;  :diameter       {:min 11.0, :q1 70.0, :median 85.0, :q3 96.0, :max 130.0, :iqr 26.0},
;;  :whole-weight   {:min 0.4,:q1 88.39417977855477,:median 159.9038757763975,:q3 230.66032011466794,:max 565.1,:iqr 142.26614033611315},
;;  :viscera-weight {:min 0.1,:q1 18.644969969969964,:median 34.12173913043479,:q3 50.57916666666666,:max 152.0,:iqr 31.934196696696695},
;;  :length         {:min 15.0, :q1 90.0, :median 109.0, :q3 123.0, :max 163.0, :iqr 33.0},
;;  :shucked-weight {:min 0.2,:q1 37.21798245614035,:median 67.18149867374008,:q3 100.43694513371933,:max 297.6,:iqr 63.218962677578986},
;;  :height         {:min 0.0, :q1 23.0, :median 28.0, :q3 33.0, :max 226.0, :iqr 10.0}}


(def standard-normal (kixi.dist/normal {:mu 0, :sd 1}))
(def standard-normal-sample (kixi.dist/sample 10000 standard-normal))

(transduce identity kixi/summary standard-normal-sample)
;; => {:min -4.352167869489498,
;;     :q1 -0.6920787623387563,
;;     :median -0.0030228476507594263,
;;     :q3 0.6797603592944492,
;;     :max 3.672515858986061,
;;     :iqr 1.3718391216332055}



(g/open (g/histogram {:data standard-normal-sample}))
(g/open (g/box-plot {:data standard-normal-sample}))

(g/open (g/vbox (g/histogram {:data standard-normal-sample
                              :bin-count 50
                              :left-margin 70
                              :x-domain [-5 5]})
                (g/box-plot {:data standard-normal-sample
                             :left-margin 70
                             :x-domain [-5 5]})))


(let [{:keys [q1 iqr q3]}
      (transduce identity kixi/summary standard-normal-sample)]
  (count (remove #(< (- q1 (* 1.5 iqr)) % (+ q3 (* 1.5 iqr))) standard-normal-sample)))
;; => 68



(def remove-outliers (filter #(< 0 (:height %) 100)))

(g/open (g/scatter-plot {:data (sequence remove-outliers abalone/data)
                         :x (comp :height)
                         :y (comp :whole-weight)}))

(let [plot #(g/barcode-plot {:data (sequence remove-outliers abalone/data) :x %})]
  (g/open
   (g/vbox
    (plot (comp (g/jitter 0.5) :length))
    (plot (comp (g/jitter 0.5) :diameter))
    (plot (comp (g/jitter 0.5) :height))
    (plot (comp g/ln :shell-weight))
    (plot (comp g/ln :viscera-weight))
    (plot (comp g/ln :shucked-weight))
    (plot (comp g/ln :whole-weight)))))






(let [variables {:rings :rings
                 :log-rings (comp g/ln :rings)
                 :length :length
                 :diameter :diameter
                 :height :height
                 :whole-weight :whole-weight
                 :shucked-weight :shucked-weight
                 :viscera-weight :viscera-weight
                 :shell-weight :shell-weight
                 :log-whole-weight (comp g/ln :whole-weight)
                 :log-shucked-weight (comp g/ln :shucked-weight)
                 :log-viscera-weight (comp g/ln :viscera-weight)
                 :log-shell-weight (comp g/ln :shell-weight)}]
  (->> abalone/data
       (transduce remove-outliers (kixi/correlation-matrix variables))
       (filter #(contains? #{:rings :log-rings} (second (key %))))
       (sort-by val)
       reverse))

(g/open
 (g/scatter-plot {:data (sequence remove-outliers abalone/data)
                  :x (comp g/ln :shell-weight)
                  :y (comp g/ln :rings)}))

(transduce remove-outliers
           (kixi/simple-linear-regression (comp g/ln :shell-weight)
                                          (comp g/ln :rings))
           abalone/data)
;; => [1.1890320511416592 0.29143092300917883]

(defn linear-model [fx fy]
  (redux/post-complete (kixi/simple-linear-regression fx fy)
                       (fn [[b a]]
                         #(+ b (* a %)))))

(def height->rings
  (transduce remove-outliers (linear-model :height :rings) abalone/data))

(transduce remove-outliers
           (kixi/rmse (comp height->rings :height) :rings)
           abalone/data)
;; => 2.5548134055728737

(def log-shell-weight->log-rings
  (transduce remove-outliers
             (linear-model (comp g/ln :shell-weight)
                           (comp g/ln :rings))
             abalone/data))

(g/exp
 (transduce remove-outliers
            (kixi/rmse (comp log-shell-weight->log-rings g/ln :shell-weight)
                       (comp g/ln :rings))
            abalone/data))
;; => 1.2397025210983978

(let [graph {:data (sequence remove-outliers abalone/data)
             :x    (comp g/ln :shell-weight)
             :y    (comp g/ln :rings)}]
  (g/open (g/overlay (g/scatter-plot graph)
                     (g/function-plot (assoc graph :fx log-shell-weight->log-rings)))))


(let [graph {:data (sequence remove-outliers abalone/data)
             :x    (comp g/ln :shell-weight)
             :y    #(- (log-shell-weight->log-rings (g/ln (:shell-weight %)))
                       (g/ln ((g/jitter 0.5) (:rings %))))
             :fx   (constantly 0)}]
  (g/open (g/histogram graph)))




























