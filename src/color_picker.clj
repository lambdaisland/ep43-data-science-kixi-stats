(ns color-picker
  (:import [java.awt Robot MouseInfo Toolkit Rectangle BorderLayout Cursor Dimension]
           [java.awt.event MouseAdapter MouseEvent]
           [javax.swing ImageIcon JFrame JWindow JLabel]))

(defn mouse-location []
  (let [point (.getLocation (MouseInfo/getPointerInfo))]
    [(.getX point) (.getY point)]))

(defn screen-size []
  (.getScreenSize (Toolkit/getDefaultToolkit)))

(defn color-at-point [[x y]]
  (let [color (.getPixelColor (Robot.) x y)]
    [(.getRed color) (.getGreen color) (.getBlue color)]))

(defn screenshot []
  (.createScreenCapture (Robot.) (Rectangle. (screen-size))))

(let [window (JFrame. "Xmessage")
      image  (JLabel. (ImageIcon. (screenshot)))]
  (doto (.getContentPane window)
    (.setLayout (BorderLayout.))
    (.add image BorderLayout/CENTER)
    (.setCursor (Cursor. (Cursor/CROSSHAIR_CURSOR)) ))
  (doto image
    (.addMouseListener
     (proxy [MouseAdapter] []
       (mouseClicked [^MouseEvent e]
         (println (apply format "%-3d    %-3d    %-3d" (color-at-point (mouse-location))))
         ))))

  (doto window
    (.setLocation 0 0)
    (.setUndecorated true)
    (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
    (.setSize (screen-size))
    (.setVisible true)))


#_
(color-at-point
 (mouse-location))
