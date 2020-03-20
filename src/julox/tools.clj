(ns julox.tools
  (:require [clojure.set :refer [union]]))

(def exclude (union (set (map char (range 91 97)))
                    (set (map char (range 58 64)))))

(defn rand-str [n]
  (->> (repeatedly n #(char (+ (* (rand) (- 122 48))
                               48)))
       (filter #(not (exclude %)))))

(defn rand-num [n]
  (* (rand) n))
