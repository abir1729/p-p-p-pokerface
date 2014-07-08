(ns p-p-p-pokerface)

(def char->int {\T 10 \J 11 \Q 12 \K 13 \A 14})

(char->int \Q)

(Integer/valueOf "0")

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
  (map suits hand))

(suit "5H")
(rank "AH")

((partial mod 15) 5)
(mod 15 5)

({:a 1 :b 2} :c 0)

(defn frequencies [nums]
  (reduce (fn [map, val] (assoc map val (inc (map val 0))))
          {}
          nums))

(frequencies [1 3 3 4 4 1])

(rest [1 2 3])

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

(empty? (rest [1]))

(any? (vals (frequencies [1 3 3 4 4 1])) 2)

(defn pair? [hand]
  (any? (vals (frequencies (ranks hand))) 2))

(any? (vals (frequencies (ranks high-seven))) 2)

(pair? pair-hand)  ;=> true
(pair? high-seven) ;=> false

(ranks pair-hand)
(any? (vals (frequencies (ranks high-seven))) 2)

(any? [1 1 1 1 1] 2)

(zipmap [1 2 3] (repeat true))

(some {3 true} [3])

(#{3} 4)

(defn check-n-in-hand [n]
  (fn [hand]
    (any? (vals (frequencies (ranks hand))) n)))

(defn three-of-a-kind? [hand]
  ((check-n-in-hand 3) hand))


(defn four-of-a-kind? [hand]
  (any? (vals (frequencies (ranks hand))) 4))

(four-of-a-kind? two-pairs-hand)      ;=> false
(four-of-a-kind? four-of-a-kind-hand) ;=> true

(three-of-a-kind? two-pairs-hand)       ;=> false
(three-of-a-kind? three-of-a-kind-hand) ;=> true



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
