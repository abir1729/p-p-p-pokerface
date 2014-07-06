(ns p-p-p-pokerface)

(def char->int {\T 10 \J 11 \Q 12 \K 13 \A 14})

(char->int \Q)

(Integer/valueOf "0")

(defn rank [card]
  (let [[rank suit] card]
    (cond
     (Character/isDigit rank) (Integer/valueOf (str rank))
     :else (char->int rank))))

(defn suit [card]
  (let [[rank suit] card]
    (str suit)))

(suit "5H")
(rank "AH")

(defn pair? [hand]
  nil)

(defn three-of-a-kind? [hand]
  nil)

(defn four-of-a-kind? [hand]
  nil)

(defn flush? [hand]
  nil)

(defn full-house? [hand]
  nil)

(defn two-pairs? [hand]
  nil)

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
