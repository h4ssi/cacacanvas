; Copyright (c) Florian Hassanen. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns h4ssi.cacacanvas)

(defn- font [] (java.awt.Font. java.awt.Font/MONOSPACED java.awt.Font/PLAIN 20))

(defn- font-bounds [f]
  (let [m (-> (proxy [javax.swing.JComponent] [])
              (.getFontMetrics f))]
    [(.charWidth m \space) (+ (.getAscent m) (.getDescent m)) (.getAscent m)]))

(font-bounds (font))

(defn cacacanvas []
  (let [[fw fh fa] (font-bounds (font))
        w          (* 10 fw)
        h          (* 10 fh)]
    (doto (proxy [javax.swing.JComponent] []
            (isOpaque [] true)
            (paintComponent [g]
                            (doseq [ww (range 10)
                                    hh (range 10)]
                              (.drawRect g (* ww fw) (* hh fh) fw fh)
                              (.setFont g (font))
                              (.drawString g "&sdfghjmMö" 0 fa))))
      (.setMinimumSize (java.awt.Dimension. w h))
      (.setMaximumSize (java.awt.Dimension. w h))
      (.setPreferredSize (java.awt.Dimension. w h))
      (.setForeground java.awt.Color/RED))))

(defn test-frame []
  (doto
    (javax.swing.JFrame. "hello")
    (.add (cacacanvas) java.awt.BorderLayout/CENTER)
    (.pack)
    (.setVisible true)))

(test-frame)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
