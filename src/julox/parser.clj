(ns julox.parser
  (:require [julox.tools :as tools]))

{:expr #{:binary :grouping :literal :unary}

:binary [:expr :operator :expr]

:grouping ["(" :expr ")"]

:literal #{:number :string "true" "false" "nil"}

:operator #{"==" "!=" "<" "<=" ">" ">=" "+" "-" "*" "/"}

:unary #{:operator :expr}

:string tools/rand-str

:number tools/rand-num}

{:expr            [:equality]
 :equality        [:comparison (repeat [#{"!=" "=="} :comparison])]
 :comparison      [:addition (repeat [#{">" ">=" "<" "<="} :addition])]
 :addition        [:multiplication (repeat [#{"+" "-"} :multiplication])]
 :multiplication  [:unary (repeat [#{"/" "*"} :unary])]
 :unary           #{[#{"!" "-"} :unary] :primary}
 :primary         #{:number :string "false" "true" "nil" ["(" :expr ")"]}}

(let [token (first tokens)
      rule (grammar rule-name)]
  (cond (vec? rule) ()))

{:expr (constantly :equality)
 :equality ()}

(defmulti match (fn [grammar] (type msg)))
(defmethod hello clojure.lang.PersistentVector
  [msg]
  (print (apply str msg)))

(defmethod hello clojure.lang.PersistentHashSet
  [msg]
  (hello (vec msg)))
(defn check-type [token typ]
  ())
(defn check-value [token value])

(hello ["ciao" "fulippo"])
(hello #{"ciao" "fulippo"})
(defn match [ast token])
(def s #{:a :b :c})
(* (rand) (count s)) (vec #{:a :b})
