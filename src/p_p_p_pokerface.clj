(ns p-p-p-pokerface)

(def char->int {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[rank suit] card]
    (cond
     (Character/isDigit rank) (Integer/valueOf (str rank))
     :else (char->int rank))))

(defn ranks [hand]
  (map rank hand))

(defn suit [card]
  (let [[rank suit] card]
    (str suit)))

(defn suits [hand]
  (map suit hand))

(defn frequencies [nums]
  (reduce (fn [map, val] (assoc map val (inc (map val 0))))
          {}
          nums))

(defn consecutive? [nums]
  (let [[first second _] nums]
    (cond
     (empty? nums) true
     (empty? (rest nums)) true
     (== second (inc first)) (consecutive? (rest nums))
     :else false)))

;(consecutive? [1 2 1])


(def high-seven                   ["2H" "3S" "4C" "5C" "7D"])
(def pair-hand                    ["2H" "2S" "4C" "5C" "7D"])
(def two-pairs-hand               ["2H" "2S" "4C" "4D" "7D"])
(def three-of-a-kind-hand         ["2H" "2S" "2C" "4D" "7D"])
(def four-of-a-kind-hand          ["2H" "2S" "2C" "2D" "7D"])
(def straight-hand                ["2H" "3S" "6C" "5D" "4D"])
(def low-ace-straight-hand        ["2H" "3S" "4C" "5D" "AD"])
(def high-ace-straight-hand       ["TH" "AS" "QC" "KD" "JD"])
(def flush-hand                   ["2H" "4H" "5H" "9H" "7H"])
(def full-house-hand              ["2H" "5D" "2D" "2C" "5S"])
(def straight-flush-hand          ["2H" "3H" "6H" "5H" "4H"])
(def low-ace-straight-flush-hand  ["2D" "3D" "4D" "5D" "AD"])
(def high-ace-straight-flush-hand ["TS" "AS" "QS" "KS" "JS"])


(defn any? [nums num]
  (cond
   (empty? nums) false
   (== (first nums) num) true
   :else (any? (rest nums) num)))

(defn pair? [hand]
  (any? (vals (frequencies (ranks hand))) 2))

(defn n-of-a-kind [n]
  (fn [hand]
    (any? (vals (frequencies (ranks hand))) n)))

(defn three-of-a-kind? [hand]
  ((n-of-a-kind 3) hand))


(defn four-of-a-kind? [hand]
  ((n-of-a-kind 4) hand))

(defn flush? [hand]
  (== 1 (count (set (suits hand)))))

(defn full-house? [hand]
  (and
   (pair? hand)
   (three-of-a-kind? hand)))

;(vals (frequencies (ranks full-house-hand)))
;(full-house? three-of-a-kind-hand) ;=> false
;(full-house? full-house-hand)      ;=> true

(defn two-pairs? [hand]
  (let [[first second third] (sort (vals (frequencies (ranks hand))))]
    (or (four-of-a-kind? hand)
        (and
         (== 2 second)
         (== 2 third)))))

(defn unique-ranks [hand]
  (set (ranks hand)))
(defn count-unique-ranks [hand]
  (count (unique-ranks hand)))

;(vals (frequencies (ranks two-pairs-hand)))
;(two-pairs? two-pairs-hand)      ;=> true
;(two-pairs? pair-hand)           ;=> false
;(two-pairs? four-of-a-kind-hand) ;=> true

(defn straight? [hand]
  (cond
   (not (== 5 (count-unique-ranks hand))) false
   (consecutive? (sort (ranks hand))) true
   (consecutive? (sort (replace {14 1}(ranks hand)))) true
   :else false))

;(sort (replace {14 1} (keys (frequencies [2 3 4 14]))))
;(straight? two-pairs-hand)             ;=> false
;(straight? straight-hand)              ;=> true
;(straight? low-ace-straight-hand)      ;=> true
;(straight? ["2H" "2D" "3H" "4H" "5H"]) ;=> false
;(straight? high-ace-straight-hand)     ;=> true

(defn straight-flush? [hand]
  (and
   (straight? hand)
   (flush? hand)))

;(straight-flush? straight-hand)                ;=> false
;(straight-flush? flush-hand)                   ;=> false
;(straight-flush? straight-flush-hand)          ;=> true
;(straight-flush? low-ace-straight-flush-hand)  ;=> true
;(straight-flush? high-ace-straight-flush-hand) ;=> true

(defn value [hand]
  nil)
