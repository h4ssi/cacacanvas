; Copyright (c) Florian Hassanen. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns h4ssi.cacacanvas
  #_(:require [clojure.tools.trace :refer :all])
  (:require [clojure.string :as s]))

#_(trace-ns 'h4ssi.cacacanvas)

(defn- font [] (java.awt.Font. java.awt.Font/MONOSPACED java.awt.Font/PLAIN 20))

(defn- font-bounds [f]
  (let [w         (javax.swing.JWindow.)
        c         (proxy [javax.swing.JComponent] [])
        [w h asc] (try
                    (doto w
                      (.setFocusableWindowState false)
                      (.add c java.awt.BorderLayout/CENTER)
                      (.setVisible true))
                    (let [g (.getGraphics c)
                          m (.getFontMetrics g f)
                          b (.getStringBounds m " " g)]
                      [(.getWidth b) (.getHeight b) (.getAscent m)])
                    (finally (.dispose w)))]
    [(Math/round w) (Math/round h) asc]))

#_(font-bounds (font))

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
                       (char-at (swap! state zdec))))

       (setIndex [_ new-index]
                 (when-not (<= 0 new-index length) (throw (IllegalArgumentException. (str "new-index must be within (" 0 "," length ")"))))
                 (reset! state new-index)
                 (char-at new-index))

       (getAllAttributeKeys [_] (-> fw-keywords keys set))

       (getAttribute [this attr] (get (.getAttributes this) attr))

       (getAttributes [_] (let [current (get caca @state)
                                fg      (:foreground-color current)
                                bg      (:background-color current)
                                attrs   {java.awt.font.TextAttribute/FAMILY java.awt.Font/MONOSPACED
                                         java.awt.font.TextAttribute/SIZE   20}
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

(defrecord CacaFrame [width height caca-iterators default-sym default-fg default-bg])
(defprotocol CacaCanvas
  (render-caca-frame [this caca-frame]))

(declare char->Color)

(defn cacacanvas
  ([] (cacacanvas nil))
  ([frame]
   (let [[fw fh fa] (font-bounds (font))
         frame      (atom frame)
         size       #(let [f @frame]
                       (if f
                         (java.awt.Dimension. (* fw (:width f)) (* fh (:height f)))
                         (java.awt.Dimension. 0 0)))]
     (proxy [javax.swing.JPanel h4ssi.cacacanvas.CacaCanvas] []
       (isOpaque [] true)
       (paintComponent [g]
                       (proxy-super paintComponent g)
                       (let [f @frame]
                         (when f
                           (doseq [[hh ascii] (map vector (range (:height f)) (:caca-iterators f))]
                             (.drawString g ascii 0 (+ fa (* fh hh)))))))
       (render_caca_frame [f]
                          (reset! frame f)
                          (doto this
                            (.setForeground (char->Color (:default-fg f)))
                            (.setBackground (char->Color (:default-bg f)))
                            (.repaint)))
       (getMinimumSize [] (size))
       (getPreferredSize [] (size))))))

(defn test-window [caca-frame]
  (let [canvas (cacacanvas caca-frame)
        window (doto
                 (javax.swing.JFrame. "hello")
                 (.setDefaultCloseOperation javax.swing.WindowConstants/DISPOSE_ON_CLOSE)
                 (.add canvas java.awt.BorderLayout/CENTER)
                 (.pack)
                 (.setVisible true))]
    [canvas window]))

(def gray-index (map (comp char (partial + (long \0))) (range 8)))
(def color-index (interleave
                  (map (comp char (partial + (long \a))) (range 26))
                  (map (comp char (partial + (long \A))) (range 26))))

(defn gray-shades []
  (let [num-of-shades (count gray-index)]
    (cons
     java.awt.Color/WHITE
     (rseq
      (mapv #(java.awt.Color. (int %) (int %) (int %))
            (take (dec num-of-shades) (iterate (partial + (/ 255 (dec num-of-shades))) 0)))))))

#_(gray-shades)

(defn colors []
  (let [shades-per-color (/ (count gray-index) 2)
        num-of-colors    (/ (count color-index) shades-per-color)
        shades           (->> (iterate (partial + (/ 1.0 (+ shades-per-color 2))) 0)
                              (drop 2) ; drop black and indistinguishable dark tones
                              (take shades-per-color)
                              (reverse))
        colors           (->> (iterate (partial + (/ 1.0 num-of-colors)) 0)
                              (take num-of-colors))]
    (map #(java.awt.Color/getHSBColor %1 %2 %3)
         (apply concat (map (partial repeat shades-per-color) colors))
         (repeat 1.0)
         (apply concat (repeat shades)))))

#_(colors)

(def index (zipmap (concat gray-index color-index) (concat (gray-shades) (colors))))

(defn char->Color [c] (get index c))

(defn spaces-to-colors
  ([color-char-seq]
   (loop [[c & cs] color-char-seq
          rep      0]
     (case c
       nil
       (repeat rep \0)

       \space
       (recur cs (inc rep))

       (concat (repeat (inc rep) c) (lazy-seq (spaces-to-colors cs c))))))
  ([[c & cs] prev-col]
   (case c
     nil
     nil

     \space
     (cons prev-col (lazy-seq (spaces-to-colors cs prev-col)))

     (cons c (lazy-seq (spaces-to-colors cs c))))))

(defn frame-from-strings
  ([sym-string fg-string bg-string]
   (frame-from-strings nil nil sym-string fg-string bg-string nil nil nil))
  ([w sym-string fg-string bg-string]
   (frame-from-strings w nil sym-string fg-string bg-string nil nil nil))
  ([w sym-string fg-string bg-string default-sym default-fg default-bg]
   (frame-from-strings w nil sym-string fg-string bg-string default-sym default-fg default-bg))
  ([w h sym-string fg-string bg-string]
   (frame-from-strings w h sym-string fg-string bg-string nil nil nil))
  ([w h sym-string fg-string bg-string default-sym default-fg default-bg]
   (let [split-string #(s/split % #"(\n?\r|\r?\n)")
         sym-strings   (split-string sym-string)
         fg-strings    (split-string fg-string)
         bg-strings    (split-string bg-string)
         w             (or w (apply max (concat (map count sym-strings) (map count fg-strings) (map count bg-strings))))
         split-w       #(mapcat (partial partition w w nil) %)
         sym-strings   (split-w sym-strings)
         fg-strings    (split-w fg-strings)
         bg-strings    (split-w bg-strings)
         h             (or h (apply max (map count [sym-strings fg-strings bg-strings])))
         pad-with      (fn [pad-item items] (concat items (repeat pad-item)))
         pad-string    (fn [pad-char string] (pad-with pad-char (seq string)))
         pad-sym       (partial pad-string (or default-sym \space))
         pad-fg        (partial pad-string (or default-fg \space))
         pad-bg        (partial pad-string (or default-bg \0))
         pad-strings   (fn [pad-per-line lines] (map pad-per-line (pad-with "" lines)))
         crop-strings  (fn [lines] (map (partial take w) (take h lines)))
         normalize     (comp crop-strings pad-strings)
         sym-strings   (normalize pad-sym sym-strings)
         fg-strings    (normalize pad-fg fg-strings)
         bg-strings    (normalize pad-bg bg-strings)
         fg-strings    (map spaces-to-colors fg-strings)
         bg-strings    (map spaces-to-colors bg-strings)
         to-colors     (partial map (partial get index))
         to-caca-chars #(mapv ->CacaChar %1 (to-colors %2) (to-colors %3))
         to-iterators  (partial map (comp caca-iterator compile-caca-chars to-caca-chars))  ]
     (->CacaFrame w h
                  (to-iterators sym-strings fg-strings bg-strings)
                  default-sym default-fg default-bg))))

(defrecord CacaSegment [symbols fg-pattern bg-pattern])

(defn segment->strings [{:keys [symbols fg-pattern bg-pattern]}]
  (let [to-string #(apply str (take (count symbols) (apply concat (repeat (seq %)))))
        fg-string (to-string fg-pattern)
        bg-string (to-string bg-pattern)]
    [symbols fg-string bg-string]))

#_(segment->strings (->CacaSegment "florian" "fl" "g"))

(defn segments->strings [segments]
  (let [strings     (map segment->strings segments)
        grep        (fn [index] (map #(get % index) strings))
        sym-strings (grep 0)
        fg-strings  (grep 1)
        bg-strings  (grep 2)]
    [(apply str sym-strings) (apply str fg-strings) (apply str bg-strings)]))

#_(segments->strings [(->CacaSegment "florian" "fl" "g") (->CacaSegment "hassanen" "ha" "i")])

(defn frame-from-segments
  ([segments] (frame-from-segments nil nil nil))
  ([segments default-sym default-fg default-bg] (frame-from-segments nil segments default-sym default-fg default-bg))
  ([w segments] (frame-from-segments w segments nil nil nil))
  ([w segments default-sym default-fg default-bg]
   (let [[sym-string fg-string bg-string] (segments->strings segments)]
     (frame-from-strings w sym-string fg-string bg-string default-sym default-fg default-bg))))

#_(frame-from-segments 3 [(->CacaSegment "florian" "fl" "g") (->CacaSegment "hassanen" "ha" "i")])

(defn append-strings-to-frame
  ([frame sym-string fg-string bg-string] (append-strings-to-frame nil frame sym-string fg-string bg-string))
  ([h {:keys [width height caca-iterators default-sym default-fg default-bg] :as frame} sym-string fg-string bg-string]
   (let [{appended-height :height
          appended-caca-iterators :caca-iterators} (frame-from-strings
                                                    width h
                                                    sym-string fg-string bg-string
                                                    default-sym default-fg default-bg)]
     (assoc frame
       :height (+ height appended-height)
       :caca-iterators (concat caca-iterators appended-caca-iterators)))))

(defn caca-tree []
  (frame-from-strings
   (str " @@ \n"
        "@@@@\n"
        "@@@@\n"
        " HH \n"
        " HH \n"
        "\"\"\"\"")
   (str " II \n"
        "IIII\n"
        "IIII\n"
        " dd \n"
        " dd \n"
        "JJJJ")
   (str "oiio\n"
        "iiii\n"
        "iiii\n"
        "oDDo\n"
        "oDDo\n"
        "DDDD")))

(defn caca-shape []
  (frame-from-strings
   (str " @@@@ \n"
        "@x@@x@\n"
        "@@@@@@\n"
        " @@@@ ")
   (str " IIII \n"
        "IaIIaI\n"
        "IIIIII\n"
        " IIII ")
   (str "oiiiio\n"
        "iAiiAi\n"
        "iiiiii\n"
        "oiiiio")))

(defn caca-palette []
  (let [ks (concat gray-index color-index)]
    (frame-from-strings 4
           (apply str ks)
           (apply str (mapcat (fn [[l _ _ r]] (vector r r l l)) (partition 4 ks)))
           (apply str ks))))

(defn caca-segments []
  (frame-from-segments 10 [(->CacaSegment "A longer text which need not to be styled" "bab" "m")
                           (->CacaSegment "Followed by another segment..." "tTt" "0")]))

#_(def ccf (caca-segments))

#_(let [frame (caca-palette)
        [canvas window] (test-window frame)]
    (def ccc canvas)
    (def ccf frame)
    (def ccw window))

#_(let [frame (append-strings-to-frame ccf "asdfbs" "gggggg" "GGGGGG")]
    (def ccf frame)
    (render-caca-frame ccc frame)
    (.pack ccw))
