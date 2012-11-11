;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Clojure port of Scheme's SRFI-26 ``Curry Upon This'' macro package
;;; (strictly inferior to Clojure's built-in lambda-syntax).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;_* Declarations =====================================================
(ns cut
  (:use clojure.test))

(declare gen slot? rest-slot? eval?)

;;;_* API ==============================================================
(def ^:dynamic *eval* nil)

;; user=> (macroexpand-1 '(cut cons (+ a 1) <>))
;; (clojure.core/fn [G__241] (cons (+ a 1) G__241))
;; user=> (macroexpand-1 '(cut list 1 <> 3 <> 5))
;; (clojure.core/fn [G__244 G__245] (list 1 G__244 3 G__245 5))
;; user=> (macroexpand-1 '(cut list))
;; (clojure.core/fn [] (list))
;; user=> (macroexpand-1 '(cut list 1 <> 3 <...>))
;; (clojure.core/fn [G__250 & G__251] (apply list 1 G__250 3 G__251))
;; user=> (macroexpand-1 '(cut <> a b))
;; (clojure.core/fn [G__254] (G__254 a b))
(defmacro cut [f & xs]
  (let [[args _ body] (gen (cons f xs))]
    `(fn ~args ~body)))

;; user=> (macroexpand-1 '(cute cons (+ a 1) <>))
;; (clojure.core/let [G__296 (+ a 1)]
;;   (clojure.core/fn [G__297] (cons G__296 G__297)))
(defmacro cute [f & xs]
  (let [[args lets body] (binding [*eval* true] (gen (cons f xs)))]
    `(let ~lets (fn ~args ~body))))

;;;_* Internals ========================================================
(defn- gen
  ([exprs]
     (gen exprs [] [] ()))
  ([[first & rest] args lets body]
     (cond
      (nil? first)
      [args lets (reverse body)]

      (and (rest-slot? first) (nil? rest))
      (let [sym (gensym)]
        [(conj args '& sym) lets (conj (reverse (conj body sym)) 'apply)])

      (slot? first)
      (let [sym (gensym)]
        (recur rest (conj args sym) lets (conj body sym)))

      (and *eval* (eval? first))
      (let [sym (gensym)]
        (recur rest args (conj lets sym first) (conj body sym)))

      :else
      (recur rest args lets (conj body first)))))

(defn- slot?      [x] (= x '<>))
(defn- rest-slot? [x] (= x '<...>))
(def   eval?      list?)

;;;_* Tests ============================================================
(deftest srfi-cut
  "SRFI-26's confidence tests for cut."
  (is (= ((cut list))                      ()))
  (is (= ((cut list <...>))                ()))
  (is (= ((cut list 1))                    '(1)))
  (is (= ((cut list <>) 1)                 '(1)))
  (is (= ((cut list <...>) 1)              '(1)))
  (is (= ((cut list 1 2))                  '(1 2)))
  (is (= ((cut list 1 <>) 2)               '(1 2)))
  (is (= ((cut list 1 <...>) 2)            '(1 2)))
  (is (= ((cut list 1 <...>) 2 3 4)        '(1 2 3 4)))
  (is (= ((cut list 1 <> 3 <>) 2 4)        '(1 2 3 4)))
  (is (= ((cut list 1 <> 3 <...>) 2 4 5 6) '(1 2 3 4 5 6)))
  (is (= (let [x (ref 'wrong)
               y (cut list @x)]
           (dosync (ref-set x 'ok))
           (y))                            '(ok)))
  (is (= (let [a (ref 0)]
           (doall
            (map (cut + (dosync (alter a inc) @a) <>)
                 '(1 2)))
           @a)                             2)))

(deftest srfi-cute
  "SRFI-26's confidence tests for cute."
  (is (= ((cute list))                      ()))
  (is (= ((cute list <...>))                ()))
  (is (= ((cute list 1))                    '(1)))
  (is (= ((cute list <>) 1)                 '(1)))
  (is (= ((cute list <...>) 1)              '(1)))
  (is (= ((cute list 1 2))                  '(1 2)))
  (is (= ((cute list 1 <>) 2)               '(1 2)))
  (is (= ((cute list 1 <...>) 2)            '(1 2)))
  (is (= ((cute list 1 <...>) 2 3 4)        '(1 2 3 4)))
  (is (= ((cute list 1 <> 3 <>) 2 4)        '(1 2 3 4)))
  (is (= ((cute list 1 <> 3 <...>) 2 4 5 6) '(1 2 3 4 5 6)))
  (is (= (let [a (ref 0)]
           (doall
            (map (cute + (dosync (alter a inc) @a) <>)
                 '(1 2)))
           @a)                              1)))

(run-tests)

;;;_* Emacs ============================================================
;;; Local Variables:
;;; allout-layout: t
;;; End:
