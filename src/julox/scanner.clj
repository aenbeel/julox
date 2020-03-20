(ns julox.scanner
  (:require [clojure.set :refer [union]]
            [clojure.pprint :as pp]
            [clojure.string :as s]
            [clojure.java.io :as io]))

(defrecord Lex [source line tokens])
(defrecord Token [type value line])

(defn char-set
  "Generate set containing all characters in the range [from; to]"
  [from to]
  (set (map char (range (int from) (inc (int to))))))

(def reserved-set #{:function :if :else :return})
(def number-set (char-set \0 \9))
(def word-start-set
  (union (char-set \A \Z)
         (char-set \a \z)
         #{\_}))

(defn add-token [lex token]
  (let [line (:line lex)]
    (if (= (:type token) :ignore)
      lex
      (update lex :tokens conj (assoc token :line line)))))

(defn consume
  ([lex]
   (assoc lex :source (rest (:source lex))))
  ([lex n]
   (first (drop n (iterate consume lex)))))

(defn inc-line
  ([lex]
   (update lex :line inc))
  ([lex n]
   (first (drop n (iterate inc-line lex)))))

(defn error-msg [line message]
  (str "[line:" line "] LexerError: " message))

(def specials-fsm {:ok {:l-paren  :left-paren
                        :r-paren  :right-paren
                        :l-brace  :left-brace
                        :r-brace  :right-brace
                        :comma    :comma
                        :dot      :dot
                        :minus    :minus
                        :plus     :plus
                        :semic    :semicolon
                        :star     :star
                        :bang     :bang 
                        :bang-eq  :bang-equal
                        :eq       :equal
                        :eq-eq    :equal-equal
                        :gt       :greater
                        :gt-eq    :greater-equal
                        :less     :less 
                        :less-eq  :less-equal}
                   :tx {:init [{:via #{\(} :to :l-paren}
                               {:via #{\)} :to :r-paren}
                               {:via #{\{} :to :l-brace}
                               {:via #{\}} :to :r-brace}
                               {:via #{\,} :to :comma}
                               {:via #{\.} :to :dot}
                               {:via #{\-} :to :minus}
                               {:via #{\+} :to :plus}
                               {:via #{\;} :to :semic}
                               {:via #{\*} :to :star}
                               {:via #{\!} :to :bang}
                               {:via #{\=} :to :eq}
                               {:via #{\>} :to :gt}
                               {:via #{\<} :to :less}]
                        :bang [{:via #{\=} :to :bang-eq}]
                        :eq [{:via #{\=} :to :eq-eq}]
                        :gt [{:via #{\=} :to :gt-eq}]
                        :less [{:via #{\=} :to :less-eq}]}})

(def blanks-fsm
  {:ok {:blank :ignore}
   :tx {:init [{:via #{\space \tab \newline} :to :blank}]}})

(def comment-fsm
  {:ok {:text :comment
        :lineend :comment}
   :tx {:init [{:via #{\/} :to :slash}]
        :slash [{:via #{\/} :to :text}]
        :text [{:via #{\newline} :to :lineend}
               {:via #(not (#{\newline} %)) :to :text}]}})

(def multi-comment-fsm
  {:ok {:slash-end :multi-comment}
   :tx {:init [{:via #{\/} :to :slash-start}]
        :slash-start [{:via #{\*} :to :text}]
        :text [{:via #(not (#{\*} %)) :to :text}
               {:via #{\*} :to :star-end}]
        :star-end [{:via #{\/} :to :slash-end}
                   {:via #(not (#{\/} %)) :to :text}]}})

(def number-fsm
  {:ok {:integer :number
        :float :number}
   :tx {:init [{:via number-set :to :integer}]
        :integer [{:via number-set :to :integer}
                  {:via #{\.} :to :dot}]
        :dot [{:via number-set :to :float}]
        :float [{:via number-set :to :float}]}})


(def string-fsm
  {:ok {:closed :string}
   :tx {:init [{:via #{\"} :to :open}]
        :open [{:via #(not (#{\newline \\ \"} %)) :to :open}
               {:via #{\\} :to :esc}
               {:via #{\"} :to :closed}]
        :esc [{:via #{\\ \"} :to :open}]}})

(def word-fsm
  {:ok {:word :identifier}
   :tx {:init [{:via word-start-set :to :word}]
        :word [{:via (union word-start-set
                            number-set) :to :word}]}
   :update! #(if (reserved-set (keyword (:value %)))
               (assoc % :type :reserved)
               (identity %))})

(defn update-if [update! token]
  (if (fn? update!)
    (update! token)
    token))

(defn parse-with-fsm [source fsm]
  (loop [src source
         state :init
         value ""]
    (let [ch (first src)
          transition (first (filter #((:via %) ch)
                                    (get-in fsm [:tx state])))]
      (if (and (char? ch) transition)
        (recur (rest src)
               (:to transition)
               (str value ch))
        (when-let [token-type (get-in fsm [:ok state])]
          (update-if (:update! fsm) {:type token-type
                                     :value value}))))))

(defn parse-next-token [source]
  (let [fsms [blanks-fsm
              comment-fsm
              multi-comment-fsm
              number-fsm
              specials-fsm
              string-fsm
              word-fsm]]
    (->> fsms
         (map #(parse-with-fsm source %))
         (filter identity)
         (first))))

(defn update-lex [lex token]
  (let [value (:value token)]
    (-> lex
        (add-token token)
        (inc-line ((frequencies value) \newline 0))
        (consume (count value)))))

(defn tokenize [source]
  (loop [lex (Lex. source 1 [])]
    (let [src (:source lex)]
      (if-let [result (parse-next-token src)]
        (recur (update-lex lex result))
        (if (empty? src)
          lex
          (do
            (pp/pprint (error-msg (:line lex)
                       (str "Trying to parse: " (first (s/split (str src) #"\n")))))
            lex))))))

(comment (pp/pprint (tokenize (str "123")))
         (tokenize (str "123..4"))
         (pp/pprint (tokenize (str "(( )){} // grouping stuff"
                                   "!*+-/=<> <= == // operators")))
         (pp/pprint (tokenize (slurp (io/resource "lox/main.lx")))))

