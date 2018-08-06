(ns ep43-data-science.graphs
  (:require [thi.ng.geom.viz.core :as viz]
            [thi.ng.geom.svg.core :as svg]
            [thi.ng.geom.vector :as v]
            [thi.ng.color.core :as col]
            [thi.ng.math.core :as m :refer [PI TWO_PI]]
            [kixi.stats.core :as kixi]
            [kixi.stats.distribution :as kixi.dist]
            [net.cgrand.xforms :as x])
  (:import [java.lang Math]))
