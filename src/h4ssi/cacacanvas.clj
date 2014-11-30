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

(defrecord CacaAnimationFrame [width height caca-iterators default-sym default-fg default-bg duration])

(defn CacaFrame->CacaAnimationFrame [caca-frame duration] (map->CacaAnimationFrame (assoc caca-frame :duration duration)))

(defrecord CacaAnimation [frames loops?])

(defprotocol CacaCanvas
  (render-caca-frame [this caca-frame])
  (render-caca-animation [this caca-animation])
  (add-animation-finished-listener [this listener])
  (remove-animation-finished-listener [this listener]))

(declare char->Color)

(defn cacacanvas
  ([] (cacacanvas nil))
  ([frame]
   (let [[fw fh fa]  (font-bounds (font))
         frame       (atom frame)
         listeners   (atom #{})
         size        #(let [f @frame]
                        (if f
                          (java.awt.Dimension. (* fw (:width f)) (* fh (:height f)))
                          (java.awt.Dimension. 0 0)))
         render      (fn [this f]
                       (reset! frame f)
                           (doto this
                             (.setForeground (char->Color (:default-fg f)))
                             (.setBackground (char->Color (:default-bg f)))
                             (.repaint)))
         timer       (doto
                       (javax.swing.Timer. 1 nil)
                       (.setRepeats false))
         clear-timer (fn []
                       ( doseq [l (seq (.getActionListeners timer))] (.removeActionListener timer l))
                       (.setInitialDelay timer Integer/MAX_VALUE)
                       (.restart timer)
                       (.stop timer))
         time-frame  (fn time-frame [this [caca-frame & caca-frames] loop-frames]
                       (clear-timer)
                       (if (and (nil? caca-frame) (nil? loop-frames))
                         (doseq [l @listeners] (.actionPerformed l nil)) ; TODO proper ActionEvent
                         (let [frame-to-render (or caca-frame (first loop-frames))]
                           (render this frame-to-render)
                           (let [next-frames (if caca-frame caca-frames (next loop-frames))]
                             (.addActionListener timer (reify java.awt.event.ActionListener
                                                         (actionPerformed [_ _] (time-frame this next-frames loop-frames))))
                             (.setInitialDelay timer (:duration frame-to-render))
                             (.start timer)))))]

     (proxy [javax.swing.JPanel h4ssi.cacacanvas.CacaCanvas] []
       (paintComponent [g]
                       (proxy-super paintComponent g)
                       (let [f @frame]
                         (when f
                           (doseq [[hh ascii] (map vector (range (:height f)) (:caca-iterators f))]
                             (.drawString g ascii 0 (+ fa (* fh hh)))))))
       (render_caca_frame [f]
         (clear-timer)
         (render this f))
       (render_caca_animation [{:keys [frames loops?]}]
                              (time-frame this frames (if loops? frames nil)))
       (add_animation_finished_listener [l] (swap! listeners conj l))
       (remove_animation_finished_listener [l] (swap! listeners disj l))
       (getMinimumSize [] (size))
       (getPreferredSize [] (size))))))

(declare frame-from-segments)
(declare ->CacaSegment)

(defprotocol CacaTextField
  (current-text
   [this]
   [this new-text]
   [this new-text cursor-position])
  (add-change-listener [this listener])
  (remove-change-listener [this listener])
  (add-return-listener [this listener])
  (remote-return-listener [this listener]))

(defn caca-text-field [w default-fg default-bg]
  (let [text      (atom {:left [] :right nil})
        change-ls (atom #{})
        return-ls (atom #{})
        blinking  (atom false)
        color     (atom nil)
        compl     #(let [rgb (seq (.getRGBColorComponents % nil))
                         m   (apply min rgb)
                         M   (apply max rgb)
                         l   (* 0.5 (+ m M))]
                     (if (> l 0.5)
                       java.awt.Color/BLACK
                       java.awt.Color/WHITE))
        text-str  #(let [{:keys [left right]} %] (apply str (concat left right)))
        typed     #(update-in %1 [:left] conj %2)
        pop-empty #(if (seq %) (pop %) %)
        backspace #(update-in % [:left] pop-empty)
        delete    #(update-in % [:right] next)
        go-left   (fn [{:keys [left right] :as text}]
                    (if (empty? left)
                      text
                      {:left (pop left)
                       :right (conj right (peek left))}))
        go-right  (fn [{:keys [left right] :as text}]
                    (if (empty? right)
                      text
                      {:left (conj left (first right))
                       :right (next right)}))
        go-home   (fn [{:keys [left right] :as text}]
                    (if (empty? left)
                      text
                      {:left [] :right (concat left right)}))
        go-end    (fn [{:keys [left right] :as text}]
                    (if (empty? right)
                      text
                      {:left (vec (concat left right)) :right nil}))
        canvas    (cacacanvas)
        fit       #(let [size (.getPreferredSize canvas)
                         w    (.-width size)
                         h    (.-height size)]
                     (.setBounds canvas % 0 w h))
        [font-w
         font-h
         & _]     (font-bounds (font))
        size      (java.awt.Dimension. (* w font-w) font-h)
        positions (fn [text] (let [left       (:left text)
                                   left-pos   (count left)
                                   offset-pos (-> left-pos (- (dec w)) (max 0))
                                   cursor-pos (- left-pos offset-pos)]
                               [offset-pos cursor-pos]))
        update    (fn [] (let [text       @text
                               text-str   (text-str text)
                               text-str   (if (= text-str "") " " text-str)
                               offset-pos (get (positions text) 0)
                               offset     (* offset-pos font-w -1)
                               frame      (frame-from-segments
                                           (max w (inc (count text-str)))
                                           [(->CacaSegment text-str (str default-fg) (str default-bg))]
                                           \space default-fg default-bg)
                               iter       (first (:caca-iterators frame))]
                           (.setIndex iter (count (:left text)))
                           (reset! color (compl (.getAttribute iter java.awt.font.TextAttribute/BACKGROUND)))
                           (render-caca-frame canvas frame)
                           (fit offset)))
        tf        (proxy [javax.swing.JPanel h4ssi.cacacanvas.CacaTextField] []
                    (getMinimumSize [] size)
                    (getPreferredSize [] size)
                    (getMaximumSize [] size)
                    (paintChildren [g]
                                   (proxy-super paintChildren g)
                                   (when @blinking
                                     (let [cursor-pos (get (positions @text) 1)]
                                       (.setColor g @color)
                                       (.drawRect g (* cursor-pos font-w) 0 (dec font-w) (dec font-h)))))
                    (current_text
                     ([] (let [text @text] (apply str (concat (:left text) (:right text)))))
                     ([new-text] (.current_text this new-text nil))
                     ([new-text cursor-pos]
                      (let [cursor-pos   (case cursor-pos
                                           nil (count (:left @text))
                                           -1  (count new-text)
                                           (min cursor-pos (count new-text)))
                            [left right] (split-at cursor-pos (seq new-text))]
                        (swap! text assoc :left (vec left) :right right)
                        (update)
                        (.repaint this))))
                    (add_change_listener [l] (swap! change-ls conj l))
                    (remove_change_listener [l] (swap! change-ls disj l))
                    (add_return_listener [l] (swap! return-ls conj l))
                    (remove_return_listener [l] (swap! return-ls disj l)))
        timer     (javax.swing.Timer. 500 (reify java.awt.event.ActionListener
                                            (actionPerformed [_ _]
                                                             (swap! blinking not)
                                                             (.repaint tf))))
        recursor  (fn [] (reset! blinking true)
                    (.restart timer)
                    (.repaint tf))
        no-cursor (fn [] (.restart timer)
                    (.stop timer)
                    (reset! blinking false)
                    (.repaint tf))]
    (.setLayout tf nil)
    (.add tf canvas)
    (update)
    (.addMouseListener tf (proxy [java.awt.event.MouseAdapter] []
                            (mouseClicked [_] (.requestFocusInWindow tf))))
    (.addFocusListener tf (reify java.awt.event.FocusListener
                            (focusGained [_ _] (recursor))
                            (focusLost [_ _] (no-cursor))))
    (.addKeyListener tf (reify java.awt.event.KeyListener
                          (keyTyped [_ e]
                                    (let [c (.getKeyChar e)
                                          got (partial = c)]
                                      (cond
                                       (got \backspace) (swap! text backspace)
                                       (got \u007f)     (swap! text delete)
                                       (got \newline)   nil
                                       :else            (swap! text typed c))
                                      (doseq [l @change-ls] (.actionPerformed l nil))
                                      (when (got \newline) (doseq [l @return-ls] (.actionPerformed l nil)))
                                      (update)
                                      (recursor)))
                          (keyPressed [_ e]
                                      (let [c (.getKeyCode e)
                                            got (partial = c)]
                                        (cond
                                         (got java.awt.event.KeyEvent/VK_LEFT) (swap! text go-left)
                                         (got java.awt.event.KeyEvent/VK_RIGHT) (swap! text go-right)
                                         (got java.awt.event.KeyEvent/VK_HOME) (swap! text go-home)
                                         (got java.awt.event.KeyEvent/VK_END) (swap! text go-end))
                                        (update)
                                        (recursor)))
                          (keyReleased [_ e])))

    (.setFocusable tf true)
    tf))

#_(let [[_ t _] (test-window (caca-palette))]
  (def ttt t))

#_(.add_change_listener ttt (reify java.awt.event.ActionListener
                              (actionPerformed [_ _] (println "type"))))
#_(.add_return_listener ttt (reify java.awt.event.ActionListener
                              (actionPerformed [_ _] (println "return"))))

#_(.current_text ttt "hoi")

(defn test-window [caca-frame]
  (let [canvas (cacacanvas caca-frame)
        tf     (caca-text-field 20 \a \0)
        window (doto
                 (javax.swing.JFrame. "hello")
                 (.setDefaultCloseOperation javax.swing.WindowConstants/DISPOSE_ON_CLOSE)
                 (.add canvas java.awt.BorderLayout/CENTER)
                 (.add tf java.awt.BorderLayout/SOUTH)
                 (.pack)
                 (.setVisible true))]
    [canvas tf window]))

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
  ([segments] (frame-from-segments segments nil nil nil))
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
        [canvas tf window] (test-window frame)]
    (add-animation-finished-listener canvas (reify java.awt.event.ActionListener
                                              (actionPerformed [_ _] (println "animation finished"))))
    (def ccc canvas)
    (def cct tf)
    (def ccf frame)
    (def ccw window))

#_(let [frame (append-strings-to-frame ccf "asdfbs" "gggggg" "GGGGGG")]
    (def ccf frame)
    (render-caca-frame ccc frame)
    (.pack ccw))

#_(render-caca-animation ccc (->CacaAnimation
                              [(CacaFrame->CacaAnimationFrame (frame-from-segments [(->CacaSegment "asdf" "X" " ")]) 500)
                               (CacaFrame->CacaAnimationFrame (frame-from-segments [(->CacaSegment "ASDF" "X" " ")]) 500)]
                              true))
