(ns money
    (:import (java.util Date Timer Random))
    (:use clojure.java.shell))
;
; utility
;
(defmacro kw [s]
    "查询当前所有ns中含特定字符串的函数，如: (kw -index)" 
    `(filter #(>= (.indexOf (str %) (name '~s)) 0)
        (sort (keys (mapcat ns-publics (all-ns))))))

(defn fib [] (map first (iterate (fn [[a b]] [b (+ a b)]) [1 1])))

(defn sys-info []
    (println (format "CPU#:%d" (.. Runtime getRuntime availableProcessors))))

(defn sys [cmd]
    (let [cmdList (clojure.string/split cmd #"\s")
           {:keys [exit out err]} (apply clojure.java.shell/sh cmdList)
           ]
        (if (= 0 exit) (println out) (println err))))

;
; stock
;
(def SXF 0.0015) ;手续费
(def YHS 0.001)  ;印花费
(def GHF 1.0)    ;过户费

(defn winG [qty pb ps]
"算股票盈利"
    (- (* qty ps (- 1 SXF YHS)) (* 2 GHF) (* qty pb (+ 1 SXF))))

(defn winQ [qty pb ps]
 "算权证盈利"
    (- (* qty ps (- 1 SXF)) (* 2 GHF) (* qty pb (+ 1 SXF))))

(defn stopLoss [qty pb lossRate]
 "止损价"
    (let [t (* qty pb (+ 1 SXF))]
         (println (format "Stop Loss at:%.2f" (- pb (/ (* t lossRate) qty))))
         (println (format "Lost Money:%.2f" (* t lossRate)))))

(defn div618 [p1 p2]
"黄金分割"
    (let [ratio '(0. 0.191 0.236 0.382 0.5 0.618 0.809 1.)
          price (fn [r] (if (<= p1 p2) (+ p1 (* (- p2 p1) r)) (- p1 (* (- p1 p2) r))))]
          (if (<= p1 p2)
            (doseq [r (reverse ratio)] (println (format "-------%.3f   %.2f-------" r (price r))))
            (doseq [r ratio] (println (format "-------%.3f  %.2f-------" r (price r)))))))

