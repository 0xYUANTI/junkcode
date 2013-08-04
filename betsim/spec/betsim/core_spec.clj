(ns betsim.core-spec
  (:use speclj.core
        betsim.core))

(describe "strategies should"
  (it "cover all bets"
      (should= bets (count strat-min)))
  (it "contain only minimum bets"
      (should= lower-limit (rand-nth strat-min)))
  (it "contain only maximum bets"
      (should= upper-limit (rand-nth strat-max)))
  (it "contain minimum and maximum bets"
      (let [strat (set (strategy))]
        (should (contains? strat lower-limit))
        (should (contains? strat upper-limit)))))

(describe "bets should"
  (it "not exceed the bankroll"
      (should= (size-bet 10.0 5.0 20.0) 5.0))
  (it "not exceed the requirement"
      (should= (size-bet 10.0 20.0 5.0) 5.0))
  (it "conform to the strategy otherwise"
      (should= (size-bet 10.0 20.0 20.0) 10.0))
  (it "win or lose"
      (let [ret (simulate-bet 100.0)]
        (should (or (= ret 0.0) (= ret 199.0))))))

(describe "simulations should"
  (it "terminate"
      (should= 10.0 (simulate () 10.0 0.0)))
  (it "go broke"
      (should= 0.0 (simulate () 0.0 100.0)))
  (it "win"
      (should (> (simulate strat-max 1000.0 100.0) 0.0))))

(describe "experiments should"
  (it "determine the optimal strategy"
      (let [min  (experiment strat-min)
            rand (experiment (strategy))
            max  (experiment strat-max)]
        (println min)
        (println rand)
        (println max)
        (should (< min rand))
        (should (< rand max)))))

;;; eof
