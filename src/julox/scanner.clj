(ns julox.scanner)

(defn init-lex [source]
  {:line 1
   :source source
   :tokens []})

(def singles
  {\( :left-paren
   \) :right-paren
   \{ :left-brace
   \} :right-brace
   \, :comma
   \. :dot
   \- :minus
   \+ :plus
   \; :semicolon
   \* :star })

(def maybe-doubles
  {\! [\= :bang-equal :bang]
   \= [\= :equal-equal :equal]
   \< [\= :less-equal :less]
   \> [\= :greater-equal :greater]})

(def multi-char-tokens
  {\( ["left-paren"]
   \) ["right-paren"]
   \{ ["left-brace"]
   \} ["right-brace"]
   \, ["comma"]
   \. ["dot"]
   \- ["minus"]
   \+ ["plus"]
   \; ["semicolon"]
   \* ["star"]
   \! ["bang" \= "-equal"]
   \= ["equal" \= "-equal"]
   \< ["less" \= "-equal"]})

\a ["bee" [\= "-too"]]
(multi-char-tokens ch)

(str (str \a) \b)



(defn concat-chars [ch source type-segments]
  (loop [chs source
         segments type-segments
         typ (str (first path))]
    (let [segment (first segments)])
    (if (= source-ch path-ch)
      (recur (rest chs) (rest path) (str identifier fragment))
      (key identifier))))
oo
(loop [source source
       path (conj ) match
       token-type ""]
  (let [ch (first source)
        frag (first path)]
    (if (= ch frag))))


{
 \! ["bang" \= ["-equal"]]}

(defn identifier [source & sets]
  (loop [source source
         sets sets
         value ""]
    (let [ch (first source)
          valid-set (first sets)]
    (if (and source (valid-set ch))
      (recur (rest source) (rest sets) (conj value ch))
      (token *missing* value)))))

(defn token [typ value]
  {:type typ
   :value value})

(defn add-token [lex token]
  (let [line (:line lex)]
    (update lex
            :tokens
            conj
            (assoc token :line line))))

(defn advance
  ([lex]
   (assoc lex :source (rest (:source lex))))
  ([lex n]
   (nth (iterate advance lex) (inc n))))

(defn inc-line [lex]
  (update lex :line inc))

(defn next-token [lex]
    (let [ch (first (:source lex))
          chs (rest (:source lex))]
      (cond
        (or (= \space ch) 
            (= \tab ch)) (advance lex)

        (= \newline ch) (inc-line (advance lex))

        (singles ch) (add-token lex (token (singles ch) ch))

        (and (maybe-doubles ch)) (select-double lex ch (rest (:source lex))) 

        :else (println "Unexpected character."))))



(defn tokenize [source]
  (loop [progress (init-progress)
         tokens []]
    (if-let [token (next-token source progress)]
      (recur progress (conj tokesn token)))))
