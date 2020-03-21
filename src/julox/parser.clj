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
  (if (or typ value)))

(defn check-type [token typ]
  ())
(defn check-value [token value])

(defn match [ast token])
(def s #{:a :b :c})
(* (rand) (count s)) (vec #{:a :b})
