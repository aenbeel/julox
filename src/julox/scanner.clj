(ns julox.scanner
    (:require [clojure.set :refer [union]])
    (:require [clojure.java.io :as io]))



(def reserved #{:function :if :else :return})



(def specials
  {\( {:type "left-paren"
       :next {}}

   \) {:type "right-paren"
       :next  {}}

   \{ {:type "left-brace"
       :next  {}}

   \} {:type "right-brace"
       :next  {}}

   \, {:type "comma"
       :next  {}}

   \. {:type "dot"
       :next  {}}

   \- {:type "minus"
       :next  {}}

   \+ {:type "plus"
       :next  {}}

   \; {:type "semicolon"
       :next  {}}

   \* {:type "star"
       :next  {}}

   \! {:type "bang" 
       :next  {\= {:type "-equal"}}}

   \= {:type "equal" 
       :next  {\= {:type "-equal"}}}

   \> {:type "greater" 
       :next  {\= {:type "-equal"}}}

   \< {:type "less" 
       :next  {\= {:type "-equal"}}}})



(def number-set (char-set \0 \9))



(def word-start-set
  (union (char-set \A \Z)
         (char-set \a \z)
         #{\_}))



(defrecord Lex [source line tokens])



(defn init-lex [source]
  (Lex. source 1 []))



(defrecord Token [type value line])



(defn make-token
  ([type value]
   (Token. type value 0))
  ([type value line]
   (Token. type value line)))



(defn add-token [lex token]
  (let [line (:line lex)]
    (update lex
            :tokens
            conj
            (assoc token :line line))))



(defn consume
  ([lex]
   (assoc lex :source (rest (:source lex))))
  ([lex n]
   (nth (iterate consume lex) n)))



(defn inc-line [lex]
  (update lex :line inc))




(defn error-msg [line message]
  (str "[line:" line "] LexerError: " message))



(defn char-set
  "Generate set containing all characters in the range [from; to]"
  [from to]
  (set (map char (range (int from) (inc (int to))))))



(defn literal-num?
  "Test whether 'ch' represents a number."
  [ch]
  (number-set ch))



(defn literal-str? 
  "Test whether 'ch' is a double quote character."
  [ch]
  (= ch \"))



(defn special?
  "Test whether char is one of the special character."
  [ch]
  (specials ch))



(defn word?
  "Test whether 'ch' is one of the admissible initial characters for a word."
  [ch]
  (word-start-set ch))



(defn make-tokenizer [start continue check assemble finalize]
  (fn [source]
    (loop [src source
           condition (start (first source))
           result (map->Token {:value ""})]
      (let [ch (first src)]
        (if (check ch condition)
          (recur (rest src)
                 (continue condition)
                 (assemble ch condition result))
          (finalize result))))))



(defn tokenize-num [source]
  (let [tokenizer
        (make-tokenizer
          (constantly number-set)
          (constantly number-set)
          (fn [ch condition] (condition ch))
          (fn [ch _ result]
            (update result :value str ch))
          (fn [result]
            (assoc result :type :number)))]
   (tokenizer source)))



(defn tokenize-str [source]
  (let [tokenizer
        (make-tokenizer
          (constantly #"[\"]")
          (constantly #"[^\"]")
          (fn [ch condition] (re-matches condition (str ch)))
          (fn [ch _ result]
            (update result :value str ch))
          (fn [result]
            (-> result
                (update :value str \")
                (assoc :type :string))))]
    (tokenizer source)))



(defn tokenize-special [source]
  (let [tokenizer
        (make-tokenizer
          (fn [ch] (first (array-map ch (specials ch))))
          (fn [condition] (first (:next (val condition))))
          (fn [ch condition] (and condition (= ch (key condition)) ))
          (fn [_ condition result]
            (-> result
                (update :type
                        (fnil str "")
                        (:type (val condition)))
                (update :value str (key condition))))
          #(update % :type keyword))]
    (tokenizer source)))



(defn tokenize-word [source]
  (let [tokenizer
        (make-tokenizer
          (constantly word-start-set)
          (constantly (union word-start-set number-set))
          (fn [ch condition] (condition ch))
          (fn [ch _ result]
            (update result :value str ch))
          (fn [result]
            (let [reserved? (reserved (keyword (:value result)))]
              (assoc result :type (if reserved? :keyword :identifier)))))]
    (tokenizer source)))



(defn next-token [lex]
  (let [src (:source lex)
        ch (first src)]
    (cond
      (or (= \space ch) 
          (= \tab ch)) (consume lex)

      (= \newline ch) (inc-line (consume lex))

      :else 
      (let [token (cond
                     (literal-num? ch) (tokenize-num src)
                     (literal-str? ch) (tokenize-str src)
                     (special? ch) (tokenize-special src)
                     (word? ch) (tokenize-word src)
                     :else (make-token "fake" "a"))]
        (-> lex
            (add-token token)
            (consume (count (:value token))))))))



(defn tokenize [source]
  (loop [lex (init-lex source)]
    (if (empty? (:source lex))
      (:tokens lex)
      (recur (next-token lex)))))


;; (clojure.pprint/pprint (tokenize (slurp (io/resource "lox/nums.lx"))))
(clojure.pprint/pprint (tokenize (slurp (io/resource "lox/main.lx"))))
