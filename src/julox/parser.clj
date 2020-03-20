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

