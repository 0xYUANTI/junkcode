(ns betsim.core
  (use incanter.core
       incanter.stats))

;; Rules
(def deposit     400.0)
(def bonus       100.0)
(def bankroll    (+ deposit bonus))
(def requirement 2500.0)
(def odds        1.99)
(def lower-limit 1.0)
(def upper-limit 100.0)

;; A strategy is a list of bets
(def bets 5000)
(defn strategy [] (sample (range lower-limit (inc upper-limit)) :size bets))
(def strat-min (repeat bets lower-limit))
(def strat-max (repeat bets upper-limit))

;; We bet as dictated by the strategy unless we're
;; out of money, or we can reach the requirement with less.
(defn size-bet [strategic bankroll requirement]
  (min strategic bankroll requirement))

(defn simulate-bet [bet] (* (first (sample [0.0 odds])) bet))

;; Simulate one run of a given strategy.
(defn simulate
  ([strategy]
     (simulate strategy bankroll requirement))
  ([strategy bankroll requirement]
     (cond
      ;; Done
      (== requirement 0.0) bankroll
      ;; Broke
      (== bankroll    0.0) 0.0
      ;; Out of bets
      (not (seq strategy))
      (throw (Exception. (format "bankroll = %f requirement = %f"
                                 bankroll
                                 requirement)))
      ;; One round of betting
      :else
      (let [bet    (size-bet (first strategy) bankroll requirement)
            return (simulate-bet bet)]
        (recur (rest strategy)
               (+ (- bankroll bet) return)
               (- requirement bet))))))

;;----------------------------------------------------------------------
(def trials 1000)

(defn experiment [strategy]
  (loop [i 0 n 0]
    (if (< i trials)
      (recur (+ i 1) (+ n (simulate strategy)))
      (/ n trials))))

;;; eof
