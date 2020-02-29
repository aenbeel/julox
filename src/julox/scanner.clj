(ns julox.scanner
    (:require [clojure.set :refer [union]])
    (:require [clojure.java.io :as io]))

(def special-words #{:if :else :return})

(def special-sequences
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

(defn init-lex [source]
  {:line 1
   :source source
   :tokens []})

(defn token [typ value]
  {:type typ
   :value value})

(defn error-msg [line message]
  (str "[line:" line "] LexerError: " message))

(defn char-set
  "Generate set containing all characters in the range [from; to]"
  [from to]
  (set (map char (range (int from) (inc (int to))))))

(def identifier-start (union (char-set \A \Z)
                             (char-set \a \z)
                             #{\_}))
(def identifier-rest (union identifier-start
                            (char-set \0 \9)))

(defn find-identifier [source sets]
  (let [initial (:first sets)
        subsequent (:rest sets)]
  ;; (assert (initial (first source))
  ;;         (error-msg "Identifier cannot start with a number"))
  (loop [src source
         value ""]
    (if (subsequent (first src))
      (recur (rest src) (str value (first src)))
      (let [normal? (not (special-words (keyword value)))]
        {:token  (token (if normal? :identifier :keyword) value)
         :count (count value)})))))


(defn find-specials [source ch info]
  (loop [src source
         segment (array-map ch info)
         typ ""
         value ""]
    (let [entry (first segment)]
      (if (and entry (= (first src) (key entry)))
        (recur (rest src)
               (:next (val entry))
               (str typ (:type (val entry)))
               (str value (key entry)))
        {:token (token (keyword typ) value)
         :count (count value)}))))

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
   (nth (iterate advance lex) n)))


(defn inc-line [lex]
  (update lex :line inc))


(defn next-token [lex]
  (let [src (:source lex)
        ch (first src)]
    (cond
      (or (= \space ch) 
          (= \tab ch)) (advance lex)

      (= \newline ch) (inc-line (advance lex))

      (multi-char-token? ch)
      (let [finder (if (special-sequences ch)
                     (fn [source]
                       (find-specials source ch (special-sequences ch)))
                     (fn [source]
                       (find-identifier source {:first identifier-start
                                                :rest identifier-rest})))
            result (finder src)]
        (-> lex
            (add-token (:token result))
            (advance (:count result))))

      :else (do (println "Unexpected character.")
                lex))))

(defn multi-char-token? [ch]
  (or (special-sequences ch)
      (identifier-start ch)))


(defn tokenize [source]
  (loop [lex (init-lex source)]
    (if (empty? (:source lex))
      (:tokens lex)
      (recur (next-token lex)))))



(clojure.pprint/pprint (tokenize (slurp (io/resource "lox/main.lx"))))
