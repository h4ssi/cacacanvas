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

(defrecord caca-char [character foreground-color background-color])

(defn- caca-iterator
  ([caca] (caca-iterator caca 0))
  ([caca state]
   (let [state   (atom state)
         caca    (vec caca)
         length  (count caca)
         char-at #(get-in caca [% :character] java.text.CharacterIterator/DONE)
         zdec    #(max 0 (dec %))
         zinc    #(min length (inc %))]
     (reify java.text.AttributedCharacterIterator
       (clone [_] (caca-iterator caca @state))
       (current [_] (char-at @state))
       (first [_]
              (reset! state 0)
              (char-at 0))
       (getBeginIndex [_] 0)
       (getEndIndex [_] length)
       (getIndex [_] @state)
       (last [_]
             (let [i (zdec length)]
               (reset! state i)
               (char-at i)))
       (next [_] (char-at (swap! state zinc)))
       (previous [_] (if (zero? @state)
                       java.text.CharacterIterator/DONE
                       (char-at (swap! state dec))))
       (setIndex [_ new-index]
                 (when-not (<= 0 new-index length) (throw (IllegalArgumentException. (str "new-index must be within (" 0 "," length ")"))))
                 (reset! state new-index)
                 (char-at new-index))))))

(def ts (caca-iterator (map #(->caca-char % nil nil) (seq "asdf"))))

(.getBeginIndex ts)
(.getEndIndex ts)
(.clone ts)
(.current ts)
(.first ts)
(.getIndex ts)
(.last ts)
(.getIndex ts)
(.next ts)
(.getIndex ts)
(.next ts)
(.getIndex ts)
(.previous ts)
(.setIndex ts 3)
(.setIndex ts 4)
;(.setIndex ts 5)

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
