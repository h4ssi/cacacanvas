; Copyright (c) Florian Hassanen. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns h4ssi.cacacanvas-test
  (:require [clojure.test :refer :all]
            [h4ssi.cacacanvas :refer :all]))

(deftest caca-iterator-test
  (let [ts (caca-iterator (compile-caca-chars
                           (mapv
                            ->CacaChar
                            (seq "asdfASDF")
                            (repeat java.awt.Color/RED)
                            (apply concat (repeat [java.awt.Color/BLUE java.awt.Color/BLUE java.awt.Color/YELLOW])))))]
    (testing "CharacterIterator implementation."
      (is (= 0 (.getBeginIndex ts)))
      (is (= 8 (.getEndIndex ts)))
      (is (= \d (.setIndex ts 2)))
      (is (= \d (.current ts)))
      (is (= \d (.current (.clone ts))))
      (is (= 2 (.getIndex (.clone ts))))
      (is (= \f (.next ts)))
      (is (= \a (.first ts)))
      (is (= java.text.CharacterIterator/DONE (.previous ts)))
      (is (= 0 (.getIndex ts)))
      (is (= \a (.current ts)))
      (doseq [_ (range 6)] (.next ts))
      (is (= \F (.next ts)))
      (is (= java.text.CharacterIterator/DONE (.next ts)))
      (is (= java.text.CharacterIterator/DONE (.current ts)))
      (is (= 8 (.getIndex ts)))
      (is (= \F (.previous ts)))
      (is (= 7 (.getIndex ts))))
    (testing "AttributedCharacteriterator implementation."
      (.first ts)
      (doseq [_ (range 2)]
        (is (= 2 (.getRunLimit ts)))
        (is (= 8 (.getRunLimit ts java.awt.font.TextAttribute/FOREGROUND)))
        (is (= 0 (.getRunStart ts)))
        (is (= 0 (.getRunStart ts java.awt.font.TextAttribute/FOREGROUND)))
        (is (= java.awt.Color/BLUE (.getAttribute ts java.awt.font.TextAttribute/BACKGROUND)))
        (is (= java.awt.Color/RED (.getAttribute ts java.awt.font.TextAttribute/FOREGROUND)))
        (.next ts))
      (is (= 2 (.getRunStart ts)))
      (is (= 3 (.getRunLimit ts)))
      (is (= 0 (.getRunStart ts #{java.awt.font.TextAttribute/FOREGROUND})))
      (is (= 8 (.getRunLimit ts #{java.awt.font.TextAttribute/FOREGROUND})))
      (is (= {java.awt.font.TextAttribute/FOREGROUND java.awt.Color/RED
              java.awt.font.TextAttribute/BACKGROUND java.awt.Color/YELLOW
              java.awt.font.TextAttribute/FAMILY java.awt.Font/MONOSPACED
              java.awt.font.TextAttribute/SIZE 20} (.getAttributes ts))))))

#_(run-tests)
