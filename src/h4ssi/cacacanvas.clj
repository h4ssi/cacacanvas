; Copyright (c) Florian Hassanen. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns h4ssi.cacacanvas
  (:require [clojure.tools.trace :refer :all]))

(trace-ns 'h4ssi.cacacanvas)

(defn- font [] (java.awt.Font. java.awt.Font/MONOSPACED java.awt.Font/PLAIN 20))

(defn- font-bounds [f]
  (let [m (-> (proxy [javax.swing.JComponent] [])
              (.getFontMetrics f))]
    [(.charWidth m \space) (+ (.getAscent m) (.getDescent m)) (.getAscent m)]))

(font-bounds (font))

(defrecord CacaChar [character foreground-color background-color])
(defrecord CompiledCacaChar [character foreground-color background-color
                             fg-run-forward bg-run-forward run-forward
                             fg-run-backward bg-run-backward run-backward])

(defn CacaChar->CompiledCacaChar [{:keys [character foreground-color background-color]}
                                  fg-run-forward bg-run-forward run-forward
                                  fg-run-backward bg-run-backward run-backward]
  (->CompiledCacaChar
   character foreground-color background-color
   fg-run-forward bg-run-forward run-forward
   fg-run-backward bg-run-backward run-backward))

(defn caca-iterator
  ([caca] (caca-iterator caca 0))
  ([caca state]
   (let [state        (atom state)
         caca         (vec caca)
         length       (count caca)
         char-at      #(get-in caca [% :character] java.text.CharacterIterator/DONE)
         zdec         #(max 0 (dec %))
         zinc         #(min length (inc %))
         fw-keywords  {java.awt.font.TextAttribute/FOREGROUND :fg-run-forward
                       java.awt.font.TextAttribute/BACKGROUND :bg-run-forward
                       java.awt.font.TextAttribute/FAMILY     :run-forward
                       java.awt.font.TextAttribute/SIZE       :run-forward}
         bw-keywords  {java.awt.font.TextAttribute/FOREGROUND :fg-run-backward
                       java.awt.font.TextAttribute/BACKGROUND :bg-run-backward
                       java.awt.font.TextAttribute/FAMILY     :run-backward
                       java.awt.font.TextAttribute/SIZE       :run-backward}]
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
                 (char-at new-index))

       (getAllAttributeKeys [_] #{java.awt.font.TextAttribute/FOREGROUND
                                  java.awt.font.TextAttribute/BACKGROUND
                                  java.awt.font.TextAttribute/FAMILY
                                  java.awt.font.TextAttribute/SIZE})

       (getAttribute [_ attr] (cond
                               (= attr java.awt.font.TextAttribute/FOREGROUND)
                               (:foreground-color (get caca @state))

                               (= attr java.awt.font.TextAttribute/BACKGROUND)
                               (:background-color (get caca @state))

                               (= attr java.awt.font.TextAttribute/FAMILY)
                               java.awt.Font/MONOSPACED

                               (= attr java.awt.font.TextAttribute/SIZE)
                               20
                               ))

       (getAttributes [_] (let [current (get caca @state)
                                fg      (:foreground-color current)
                                bg      (:background-color current)
                                attrs   {java.awt.font.TextAttribute/FAMILY java.awt.Font/MONOSPACED
                                         java.awt.font.TextAttribute/SIZE 20}
                                attrs   (if fg (assoc attrs java.awt.font.TextAttribute/FOREGROUND fg) attrs)
                                attrs   (if fg (assoc attrs java.awt.font.TextAttribute/BACKGROUND bg) attrs)]
                            attrs))

       (getRunLimit [this] (.getRunLimit this #{java.awt.font.TextAttribute/FOREGROUND java.awt.font.TextAttribute/BACKGROUND}))

       (^int getRunLimit [this ^java.text.AttributedCharacterIterator$Attribute attr]
             (.getRunLimit this #{attr}))

       (^int getRunLimit [this ^java.util.Set attrs]
             (let [index   @state
                   current (get caca index)]
               (+ index 1 (apply min (map #((get fw-keywords %) current) attrs)))))

       (getRunStart [this] (.getRunStart this #{java.awt.font.TextAttribute/FOREGROUND java.awt.font.TextAttribute/BACKGROUND}))

       (^int getRunStart [this ^java.text.AttributedCharacterIterator$Attribute attr]
             (.getRunStart this #{attr}))

       (^int getRunStart [this ^java.util.Set attrs]
             (let [index   @state
                   current (get caca index)]
               (- index (apply min (map #((get bw-keywords %) current) attrs)))))))))

(defn- get-fg-bg-runs-in-seq [s]
  (loop [[{:keys [foreground-color background-color] :as p} & ps] s
         fg-prev                                                  :no-color
         fg-run                                                   []
         bg-prev                                                  :no-color
         bg-run                                                   []]
    (if-not p
      [fg-run bg-run (vec (range (count fg-run)))]
      (let [fg-run-next (if (= foreground-color fg-prev) (inc (peek fg-run)) 0)
            bg-run-next (if (= background-color bg-prev) (inc (peek bg-run)) 0)]
        (recur ps foreground-color (conj fg-run fg-run-next) background-color (conj bg-run bg-run-next))))))

#_(get-fg-bg-runs-in-seq (map ->CacaChar (seq "asdfasdf") (repeat :blue) (apply concat (repeat [:red :red :yellow]))))
#_(get-fg-bg-runs-in-seq (rseq (mapv ->CacaChar (seq "asdfasdf") (repeat :blue) (apply concat (repeat [:red :red :yellow])))))

(defn compile-caca-chars [caca-chars]
  (let [[backward-fg-runs backward-bg-runs backward-runs] (get-fg-bg-runs-in-seq caca-chars)
        [forward-fg-runs  forward-bg-runs  forward-runs]   (map rseq (get-fg-bg-runs-in-seq (rseq caca-chars)))]
    (map CacaChar->CompiledCacaChar
         caca-chars
         forward-fg-runs
         forward-bg-runs
         forward-runs
         backward-fg-runs
         backward-bg-runs
         backward-runs)))

#_(compile-caca-chars (mapv ->CacaChar (seq "asdfasdf") (repeat :blue) (apply concat (repeat [:red :red :yellow]))))
(def ^java.text.AttributedCharacterIterator ts
  (caca-iterator
   (compile-caca-chars
    (mapv ->CacaChar
          (seq "qqqq@@Ssss")
          (apply concat (repeat [java.awt.Color/RED java.awt.Color/BLUE]))
          (apply concat (repeat [java.awt.Color/YELLOW]))))))

(defn cacacanvas []
  (let [[fw fh fa] (font-bounds (font))
        w          (* 10 fw)
        h          (* 10 fh)]
    (doto (proxy [javax.swing.JComponent] []
            (isOpaque [] true)
            (paintComponent [g]
                            #_(doseq [ww (range 10)
                                    hh (range 10)]
                              (.drawRect g (* ww fw) (* hh fh) fw fh)
                              )
                            (doseq [hh (range 10)]
                              (.drawString g ts 0 (+ fa (* fh hh))))))
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
