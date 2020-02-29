(ns julox.core
  (:require [julox.scanner :as scanner])
  (:require [clojure.java.io :as io]))

(def loxsrc "lox/main.lx")

(def evaluate evaluate1)

(defn start [& args]
  (let [argv (vec args)
        argc (count argv)]
    (cond (> argc 1) (throw-usage-message)
          (= argc 1) (run-file (argv 0))
          (= argc 0) (run-repl))))

(defn throw-usage-message []
  (println "Usage: julox [source-file]"))

(defn run-file [path]
  (evaluate (slurp (io/resource path))))

(defn run-repl []
  (repeatedly 4 #(-> (read-line)
                     (evaluate)
                     (println))))

(defn evaluate1 [source] source)

(defn evaluate2 [source]
  (doseq [token (scanner/scan source)]
    (println token)))

(defn error [line message]
  (println (str "[line" line "] Error: " message)))




