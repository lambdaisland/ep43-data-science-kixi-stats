(ns ep43-data-science.abalone
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]))

(def abalone-csv-file (io/resource "abalone.data"))

(def parse-int #(Integer/parseInt %))
(def parse-double #(Double/parseDouble %))

(defn parse-row [[sex & rst]]
  (let [rings (-> rst last parse-int)
        rst   (butlast rst)]
    (->> rst
         ;; Values in the CSV are divided by 200, convert back to milimeters and
         ;; grams.
         (map #(-> % parse-double (* 200)))
         (zipmap [:length :diameter :height :whole-weight
                  :shucked-weight :viscera-weight :shell-weight])
         (into {:sex sex :rings rings}))))

(def data
  (->> abalone-csv-file
       slurp
       csv/read-csv
       (map parse-row)))

(def numeric-variables
  [:rings :length :diameter :height :whole-weight
   :shucked-weight :viscera-weight :shell-weight])

(comment
  (take 2 (shuffle data))
  ;; => ({:rings 9,
  ;;      :sex "F",
  ;;      :shell-weight 70.6,
  ;;      :diameter 96.0,
  ;;      :whole-weight 228.7,
  ;;      :viscera-weight 46.9,
  ;;      :length 128.0,
  ;;      :shucked-weight 98.3,
  ;;      :height 39.0}
  ;;     {:rings 8,
  ;;      :sex "I",
  ;;      :shell-weight 16.0,
  ;;      :diameter 56.99999999999999,
  ;;      :whole-weight 47.4,
  ;;      :viscera-weight 7.9,
  ;;      :length 75.0,
  ;;      :shucked-weight 21.2,
  ;;      :height 18.0})
  )
