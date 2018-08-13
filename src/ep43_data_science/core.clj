(ns ep43-data-science.core
  (:require [ep43-data-science.abalone :as abalone]
            [ep43-data-science.graphs :as g]
            [kixi.stats.core :as kixi]
            [kixi.stats.distribution :as kixi.dist]
            [kixi.stats.math :as m]
            [net.cgrand.xforms :as x]
            [redux.core :as redux]))


;; Let's look at the dataset. How many data points are there?

(count abalone/data)
;; => 4177

;; What does each entry look like?

(rand-nth abalone/data)
;; => {:rings 9,
;;     :sex "I",
;;     :shell-weight 14.7,
;;     :diameter 54.0,
;;     :whole-weight 54.2,
;;     :viscera-weight 10.5,
;;     :length 71.0,
;;     :shucked-weight 28.5,
;;     :height 21.0}

;; Try some kixi.stats: what's the mean (average) amount of rings

(transduce (map :rings) kixi/mean abalone/data)
;; => 9.933684462532918

;; It's a reducing function with init step:

(kixi/mean)
;; => [0.0 0.0]

;; And completing step:

(transduce (map :rings) (completing kixi/mean identity) abalone/data)
;; => [41493.0 4177.0]

;; But more useful is the median

(transduce (map :rings) kixi/median abalone/data)
;; => 9.125

;; Five point summary

(transduce (map :rings) kixi/summary abalone/data)
;; => {:min 1.0, :q1 8.0, :median 9.282608695652174, :q3 11.0, :max 29.0, :iqr 3.0}

(into {}
      (comp cat (x/by-key key val (x/reduce kixi/summary)))
      abalone/data)

;; => {:rings
;;     {:min 1.0, :q1 8.0, :median 9.266666666666667, :q3 11.0, :max 29.0, :iqr 3.0},
;;     :sex {:min nil, :q1 nil, :median nil, :q3 nil, :max nil, :iqr nil},
;;     :shell-weight
;;     {:min 0.3,
;;      :q1 26.015904471544715,
;;      :median 46.54394268774704,
;;      :q3 65.75708502024293,
;;      :max 201.0,
;;      :iqr 39.74118054869821},
;;     :diameter
;;     {:min 11.0, :q1 70.0, :median 85.0, :q3 96.0, :max 130.0, :iqr 26.0},
;;     :whole-weight
;;     {:min 0.4,
;;      :q1 88.39417977855477,
;;      :median 159.9038757763975,
;;      :q3 230.66032011466794,
;;      :max 565.1,
;;      :iqr 142.26614033611315},
;;     :viscera-weight
;;     {:min 0.1,
;;      :q1 18.62167076167076,
;;      :median 34.11797729618162,
;;      :q3 50.57916666666666,
;;      :max 152.0,
;;      :iqr 31.9574959049959},
;;     :length
;;     {:min 15.0, :q1 90.0, :median 109.0, :q3 123.0, :max 163.0, :iqr 33.0},
;;     :shucked-weight
;;     {:min 0.2,
;;      :q1 37.21798245614035,
;;      :median 67.18149867374008,
;;      :q3 100.43694513371933,
;;      :max 297.6,
;;      :iqr 63.218962677578986},
;;     :height {:min 0.0, :q1 23.0, :median 28.0, :q3 33.0, :max 226.0, :iqr 10.0}}

;; Let's start visualizing

(g/open (g/histogram {:data abalone/data :x :rings}))


;; Custom bin count so 1 ring = 1 bin

(-> {:data abalone/data :x :rings :bin-count 29}
    g/histogram
    g/open)

;; Looks kinda normal, but skewed with a big long tail at the top end. Log-normal?
;; Try a histogram of the log values, add some jitter because now the improve the distribution across adjacent bins.

(-> {:data abalone/data
     :x (comp g/ln (g/jitter 0.5) :rings)
     :bin-count 29}
    g/histogram
    g/open)

(g/open (g/histogram {:data abalone/data
                      :x (comp g/ln :rings)
                      :bin-count 29}))

(g/open (g/histogram {:data abalone/data
                      :x (comp g/ln (g/jitter 0.5) :rings)
                      :bin-count 29}))

;; That looks normal, yay. Now we have a model for reasoning about the occurence
;; of certain ring counts in the population. It also means we'll get better
;; results when doing linear regression of the log values.

;; We want to find a variable that can predict the amount of rings, let's first
;; see how different variables correlate with the correlation matrix.

;; Correlation coefficient = Pearson Correlation Coefficient = (ssx * ssy) / ssxy

;; Indicates how well a line through the data "fits" the data. Closer to zero =
;; no clear line, close to -1 or 1 means good fit, either positive or negative

(->> data
     (transduce identity (kixi/correlation-matrix (into {} (x/for [k %] [k k]) abalone/numeric-variables)))
     (filter #(= (second (key %)) :rings))
     (sort-by val)
     reverse)
;; => ([[:shell-weight :rings] 0.6281693770553378]
;;     [[:height :rings] 0.6101065785871609]
;;     [[:diameter :rings] 0.5745507040035986]
;;     [[:length :rings] 0.5565719701768042]
;;     [[:whole-weight :rings] 0.5406206352802435]
;;     [[:viscera-weight :rings] 0.5039771816106745]
;;     [[:shucked-weight :rings] 0.42115592388147804])

;; Looks like shell-weight and :height would be interesting ones to explore. 0.6
;; isn't particularly high, but it should be high enough to predict things with
;; a certain amount of error

;; We saw previously though that it might be interesting to look into the log of
;; rings. And indeed, height has a 0.68 correlation with log-rings, that looks
;; promising!

(->> data
     (transduce identity (kixi/correlation-matrix (into {:log-rings (comp g/ln :rings)} (x/for [k %] [k k]) abalone/numeric-variables)))
     (filter #(= (second (key %)) :log-rings))
     (sort-by val)
     reverse)
;; => ([[:rings :log-rings] 0.9657620285963021]
;;     [[:height :log-rings] 0.6831670725853732]
;;     [[:diameter :log-rings] 0.6682178212148067]
;;     [[:shell-weight :log-rings] 0.6670829447054146]
;;     [[:length :log-rings] 0.6536347209423128]
;;     [[:whole-weight :log-rings] 0.5961369422707791]
;;     [[:viscera-weight :log-rings] 0.5660724194243916]
;;     [[:shucked-weight :log-rings] 0.49146737914553723])

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
  (->> data
       (transduce identity (kixi/correlation-matrix variables))
       (filter #(contains? #{:rings :log-rings} (second (key %))))
       (sort-by val)
       reverse))
;;=>
([[:log-rings :rings] 0.9657620285963021]
 [[:rings :log-rings] 0.9657620285963021]
 [[:log-shell-weight :log-rings] 0.7401395289870353]
 [[:log-whole-weight :log-rings] 0.696019947259287]
 [[:height :log-rings] 0.6831670725853732]
 [[:log-viscera-weight :log-rings] 0.6799492099739567]
 [[:diameter :log-rings] 0.6682178212148067]
 [[:shell-weight :log-rings] 0.6670829447054146]
 ,,, )

(->> data
     (transduce identity (kixi/correlation-matrix (into {:log-rings (comp g/ln :rings)} (x/for [k %] [k k]) abalone/numeric-variables)))
     (filter #(= (second (key %)) :log-rings))
     (sort-by val)
     reverse)


;; Several other of these also have a pretty high correlation with log-rings,
;; but they also all have very high correlations with each other, meaning that
;; they don't add a lot of extra information beyond what the first variable
;; provides.

(transduce identity (kixi/correlation :height :diameter) data)
;; => 0.9071865567564446

(transduce identity (kixi/correlation :height :shell-weight) data)
;; => 0.8918574899443572

(transduce identity (kixi/correlation :height :length) data)
;; => 0.900868043935849

;; Let's look at some other variables

(g/open (g/histogram {:data abalone/data
                      :x :height}))

;; This one looks a little strange, in particular notice that even though the
;; x-axis goes up to 260, all data seems to be clumped around 20 to 40. I have a
;; feeling this histogram isn't telling the whole story.

;; Let's check the box-plot. Indeed: there are two pretty extreme outliers. Are they real?

(g/open (g/box-plot {:data abalone/data
                     :x :height}))

;; Let's compare the height to the length. Maybe they really did encounter some
;; abalones that were ten times bigger than the rest, but in that case that
;; should at least show to some extent in other variables.

(g/open (g/scatter-plot {:data abalone/data
                         :x :height
                         :y :length}))

;; Ok, that's pretty clear, those two points to the far right must be mistakes
;; created during the measuring. Maybe someone wrote down a zero too many or
;; placed a comma in the wrong place.
;;
;; Let's just remove these two samples from the data.

(def data (filter #(< (:height %) 100) abalone/data))

;; Now let's check the box plot again. Now you can see that things are
;; distributed symmetrically, and clumped closely around the median.
;;
;; This looks better, but there's still something fishy. It seems a few data
;; points have a height of zero. We can assume that height is always positive,
;; so there's probably also something going on with those, let's remove those as
;; well

(def data (filter #(< 0 (:height %) 100) abalone/data))

(g/open (g/box-plot {:data data
                     :x :height}))

;; That central box is quite narrow, meaning that half of all data is clumped
;; tightly around the median. You can see the same from the five-number summary.

(transduce (map :height) kixi/summary data)
;; => {:min 2.0, :q1 23.0, :median 28.000000000000004, :q3 33.0, :max 50.0, :iqr 10.0}

;; Finally let's look at the histogram for :height again now that we've cleaned
;; up the data.

(g/open (g/histogram {:data data
                      :x :height}))

(g/render-to-org-link (g/histogram {:data data
                                    :x :height}))
;; => [[file:/tmp/ep43-data-science.graphs4900940309823888059.svg]]

;; That looks quite reasonable, it seems normal rather than log-normal.

(defn var-box-plot [var]
  (g/box-plot {:data data :x var}))

;; Box plot

(g/open (g/box-plot {:data abalone/data :x :length}))
(g/open (g/box-plot {:data abalone/data :x (comp g/ln :rings)}))

(def standard-normal (kixi.dist/normal {:sd 1 :mu 0}))
(def standard-normal-sample (kixi.dist/sample 10000 standard-normal))

(g/open (g/box-plot {:data standard-normal-sample}))

(transduce identity kixi/summary standard-normal-sample)
;; => {:min -3.8519271282006016,
;;     :q1 -0.6659900852641175,
;;     :median -0.01714498352689695,
;;     :q3 0.6686650777464612,
;;     :max 3.5555441959666005,
;;     :iqr 1.3346551630105787}


(let [{:keys [q1 iqr q3]} (transduce identity kixi/summary standard-normal-sample)]
  (count (remove #(< (- q1 (* 1.5 iqr))
                     %
                     (+ q3 (* 1.5 iqr)))
                 standard-normal-sample)))
;; => 59

(count (remove #(< (* -1.5 1.3346) % (* 1.5 1.3346)) standard-normal-sample))

(g/open (g/vbox (g/histogram {:data standard-normal-sample
                              :bin-count 50
                              :left-margin 70
                              :x-domain [-5 5]})
                (g/box-plot {:data standard-normal-sample
                             :left-margin 70
                             :x-domain [-5 5]})))
(let [{:keys [q1 iqr q3]}
      (transduce identity kixi/summary standard-normal-sample)]
  [(- q1 (* 1.5 iqr)) (+ q3 (* 1.5 iqr))])
;; => [-2.7308179100554195 2.778935492233497]

(g/open
 (g/hbox
  (apply g/vbox (map #(g/box-plot {:data abalone/data :x %})
                     [:rings
                      :length
                      :diameter
                      :height]))
  (apply g/vbox (map #(g/box-plot {:data abalone/data :x %})
                     [:whole-weight
                      :shucked-weight
                      :viscera-weight
                      :shell-weight] ))))
(g/open
 (g/hbox
  (apply g/vbox (map #(g/barcode-plot {:data data :x %})
                     [:rings
                      :length
                      :diameter
                      :height]))
  (apply g/vbox (map #(g/barcode-plot {:data data :x %})
                     [:whole-weight
                      :shucked-weight
                      :viscera-weight
                      :shell-weight] ))))

(def remove-outliers (filter #(< 0 (:height %) 100)))
(def remove-infants  (remove #(= (:sex %) "I")))

(let [plot #(g/box-plot {:data (sequence remove-outliers data) :x %})]
  (g/open
   (g/hbox
    (apply g/vbox (map plot [(comp g/ln (g/jitter 0.5) :rings)
                             :length
                             :diameter
                             :height]))
    (apply g/vbox (map plot [:whole-weight
                             :shucked-weight
                             :viscera-weight
                             :shell-weight] )))))

(g/open (g/histogram {:data data :x :whole-weight}))
(g/open (g/histogram {:data data :x :shell-weight :bin-count 100}))

(g/open (g/scatter-plot {:data abalone/data
                         :x (comp :height)
                         :y (comp :whole-weight)}))

(let [plot #(g/box-plot {:data abalone/data :x %})]
  (g/open
   (g/vbox
    (plot :length)
    (plot :height)
    (plot :diameter))))

(let [plot #(g/box-plot {:data (sequence remove-outliers
                                         abalone/data)
                         ;;                 :x-domain [0 180]
                         :x %})]
  (g/open
   (g/vbox
    (plot :length)
    (plot :height)
    (plot :diameter))))

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

(g/open (g/scatter-plot {:data (sequence remove-outliers abalone/data)
                         :x (comp :height)
                         :y (comp :whole-weight)}))

(g/open (g/scatter-plot {:data data
                         :x (comp (g/jitter 1) :height)
                         :y (comp (g/jitter 0.5) :rings)}))

(g/open (g/scatter-plot {:data data
                         :x (comp (g/jitter 1) :shell-weight)
                         :y (comp (g/jitter 0.5) :rings)}))


(g/open (g/histogram {:data (sequence (comp remove-outliers
                                            (remove #(= (:sex %) "I")))
                                      abalone/data)
                      :x :shell-weight :bin-count 100}))

(let [plot1 #(g/barcode-plot {:data (sequence (comp remove-outliers) abalone/data) :x %})
      plot2 #(g/barcode-plot {:data (sequence (comp remove-outliers remove-infants) abalone/data) :x %})
      ]
  (g/open
   (g/hbox
    (apply g/vbox (map plot1 [:whole-weight
                              :shucked-weight
                              :viscera-weight
                              :shell-weight]))
    (apply g/vbox (map plot2 [:whole-weight
                              :shucked-weight
                              :viscera-weight
                              :shell-weight] )))))

(x/count (comp remove-outliers
               (remove #(= (:sex %) "I")))
         abalone/data)


(defn create-linear-model [data fx fy]
  (let [[b a] (x/some (x/reduce
                       (kixi/simple-linear-regression fx fy))
                      data)]
    (fn [x]
      (+ b (* a x)))))

(def shell-weight->rings
  (create-linear-model (eduction remove-outliers abalone/data) :shell-weight :rings))

(transduce identity
           (kixi/rmse (comp shell-weight->rings :shell-weight) :rings)
           test-data)
;; => 2.361401737292544

(def height->rings
  (create-linear-model (eduction remove-outliers abalone/data) :height :rings))

(transduce identity
           (kixi/rmse (comp height->rings :height) :rings)
           test-data)
;; => 2.368657406667301

(def shell-weight->log-rings
  (create-linear-model training-data :shell-weight (comp g/ln :rings)))

(transduce identity
           (kixi/rmse (comp shell-weight->log-rings :shell-weight) (comp g/ln :rings))
           test-data)
;; => 0.2279903023624444

(def height->log-rings
  (create-linear-model training-data :height (comp g/ln :rings)))

(transduce identity
           (kixi/rmse (comp height->log-rings :height) (comp g/ln :rings))
           test-data)
;; => 0.2216720171528734


(def log-shell-weight->log-rings
  (create-linear-model (eduction remove-outliers abalone/data) (comp g/ln :shell-weight) (comp g/ln :rings)))

(g/exp
 (transduce remove-outliers
            (kixi/rmse (comp log-shell-weight->log-rings g/ln :shell-weight) (comp g/ln :rings))
            (eduction remove-outliers abalone/data)))
;; => 1.2270373451920258

;; Are the errors normally distributed
(g/open
 (g/scatter-plot {:data (sequence remove-outliers abalone/data)
                  :x (comp g/ln :shell-weight)
                  :y (comp g/ln :rings)
                  }))


(let [graph {:data (sequence (comp (drop 3000) remove-outliers) abalone/data)
             :x    (comp g/ln :shell-weight)
             :y    #(- (log-shell-weight->log-rings (g/ln (:shell-weight %)))
                       (g/ln ((g/jitter 0.5) (:rings %))))
             :fx   (constantly 0)
             :grid? false
             :x-visible false
             :y-visible false}]
  (g/open (assoc (g/overlay (g/scatter-plot (assoc graph
                                                   :radius (fn [_] (rand-int 10))
                                                   :fill (fn [_] (rand-nth ["#f88b33" "#fd3d3c"]))
                                                   :fill-opacity 0.5))
                            (g/function-plot graph))
                 :background "#282b4e")))

(g/open
 (g/scatter-plot {:data abalone/data
                  :x :shell-weight
                  :y :height
                  :radius #(/ (:rings %) 10)}))

(g/open (g/overlay (g/scatter-plot {:data test-data
                                    :x :height
                                    :y :rings})
                   (g/function-plot {:data test-data
                                     :x :height
                                     :y :rings
                                     :fx height->rings})))

(let [graph {:data (sequence remove-outliers test-data)
             :x    (comp g/ln :shell-weight)
             :y    (comp g/ln :rings)
             :fx   log-shell-weight->log-rings}]
  (g/open (g/overlay (g/scatter-plot graph)
                     (g/function-plot graph))))

(let [graph {:data (sequence remove-outliers test-data)
             :x    (comp g/ln :shell-weight)
             :y    #(g/exp (- (log-shell-weight->log-rings (g/ln (:shell-weight %)))
                              (g/ln ((g/jitter 0.5) (:rings %)))))}]
  (g/open (g/histogram graph)))


(defn r-squared [x y]
  (redux/post-complete (kixi/correlation x y) sq))

(transduce identity (r-squared :shell-weight :rings) data)
;; => 0.39384918134304175

(transduce identity (r-squared :height :rings) data)
;; => 0.31076981783840063
