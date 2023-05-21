(import ./grammar :prefix "")

# this looks complicated, but it's just wrapping pieces of the original
# grammar with appropriate capture constructs that also capture start and
# end positions
(def cg-capture-ast
  (let [ca (table ;(kvs cg))]
    (each kwd [:character :comment :keyword :macro-keyword :number
               :string :symbol :whitespace]
          (put ca kwd
               ~(cmt (sequence (position)
                               (capture ,(in ca kwd))
                               (position))
                     ,|[kwd {:start (first $&)
                             :end (last $&)}
                            (in $& 1)])))
    (each kwd [:backtick :conditional :conditional-splicing
               :deprecated-metadata-entry :deref :discard
               :eval :metadata :metadata-entry :namespaced-map
               :quote :tag :unquote :unquote-splicing :var-quote]
          (put ca kwd
               ~(cmt (sequence (position)
                               (capture ,(in ca kwd))
                               (position))
                     ,|[kwd {:start (first $&)
                             :end (last $&)}
                            ;(slice $& 1 -3)])))
    (each kwd [:list :map :set :vector]
          (let [original (in ca kwd)
                wrap-target (get-in ca [kwd 2])
                replacement (tuple # array needs to be converted
                              ;(put (array ;original)
                                    2 ~(capture ,wrap-target)))]
            (put ca kwd
                 ~(cmt (sequence (position)
                                 ,replacement
                                 (position))
                       ,|[kwd {:start (first $&)
                               :end (last $&)}
                              ;(slice $& 1 -3)]))))
    #
    (let [original (in ca :regex)
          wrap-target (get-in ca [:regex 2])
          replacement (tuple # array needs to be converted
                        ;(put (array ;original)
                              2 ~(capture ,wrap-target)))]
      (put ca :regex
           ~(cmt (sequence (position)
                           ,replacement
                           (position))
                 ,|[:regex {:start (first $&)
                            :end (last $&)}
                           (last (in $& 1))])))
    #
    (let [original (in ca :fn)
          wrap-target (get-in ca [:fn 2])
          replacement (tuple # array needs to be converted
                        ;(put (array ;original)
                              2 ~(capture ,wrap-target)))]
      (put ca :fn
           ~(cmt (sequence (position)
                           ,replacement
                           (position))
                 ,|[:fn {:start (first $&)
                         :end (last $&)}
                    ;(slice $& 1 -3)])))
    #
    (put ca :symbolic
         ~(cmt (sequence (position)
                         (capture ,(in ca :symbolic))
                         (position))
               ,|[:symbolic {:start (first $&)
                             :end (last $&)}
                            (last ;(slice $& 1 -3))]))
    #
    (put ca :auto-resolve
         ~(cmt (sequence (position)
                         (capture ,(in ca :auto-resolve))
                         (position))
               ,(fn [& caps]
                  [:auto-resolve {:start (first caps)
                                  :end (last caps)}])))
    ca))

(comment

  (peg/match cg-capture-ast ":a")
  # =>
  @[[:keyword {:start 0 :end 2} ":a"]]

  (peg/match cg-capture-ast "\"smile\"")
  # =>
  @[[:string {:start 0 :end 7} "\"smile\""]]

  (peg/match cg-capture-ast "1/2")
  # =>
  @[[:number {:start 0 :end 3} "1/2"]]

  (peg/match cg-capture-ast "defmacro")
  # =>
  @[[:symbol {:start 0 :end 8} "defmacro"]]

  (peg/match cg-capture-ast "::a")
  # =>
  @[[:macro-keyword {:start 0 :end 3} "::a"]]

  (peg/match cg-capture-ast "\\a")
  # =>
  @[[:character {:start 0 :end 2} "\\a"]]

  (peg/match cg-capture-ast "{}")
  # =>
  @[[:map {:start 0 :end 2}]]

  (deep=
    #
    (peg/match cg-capture-ast "{:a 1}")
    #
    @[[:map {:start 0 :end 6}
       [:keyword {:start 1 :end 3} ":a"]
       [:whitespace {:start 3 :end 4} " "]
       [:number {:start 4 :end 5} "1"]]])
  # =>
  true

  (deep=
    #
    (peg/match cg-capture-ast "#::{}")
    #
    @[[:namespaced-map {:start 0 :end 5}
       [:auto-resolve {:start 1 :end 3}]
       [:map {:start 3 :end 5}]]])
  # =>
  true

  (deep=
    #
    (peg/match cg-capture-ast "#::a{}")
    #
    @[[:namespaced-map {:start 0 :end 6}
       [:macro-keyword {:start 1 :end 4} "::a"]
       [:map {:start 4 :end 6}]]])
  # =>
  true

  (deep=
    #
    (peg/match cg-capture-ast "#:a{}")
    #
    @[[:namespaced-map {:start 0 :end 5}
       [:keyword {:start 1 :end 3} ":a"]
       [:map {:start 3 :end 5}]]])
  # =>
  true

  (peg/match cg-capture-ast "[]")
  # =>
  @[[:vector {:start 0 :end 2}]]

  (deep=
    #
    (peg/match cg-capture-ast "[:a]")
    #
    @[[:vector {:start 0 :end 4}
       [:keyword {:start 1 :end 3} ":a"]]])
  # =>
  true

  (peg/match cg-capture-ast "()")
  # =>
  @[[:list {:start 0 :end 2}]]

  (deep=
    #
    (peg/match cg-capture-ast "(:a)")
    #
    @[[:list {:start 0 :end 4}
       [:keyword {:start 1 :end 3} ":a"]]]
    ) # =>
  true

  (deep=
    #
    (peg/match cg-capture-ast "^{:a true} [:a]")
    #
    @[[:metadata {:start 0 :end 15}
       [:metadata-entry {:start 0 :end 10}
        [:map {:start 1 :end 10}
         [:keyword {:start 2 :end 4} ":a"]
         [:whitespace {:start 4 :end 5} " "]
         [:symbol {:start 5 :end 9} "true"]]]
       [:whitespace {:start 10 :end 11} " "]
       [:vector {:start 11 :end 15}
        [:keyword {:start 12 :end 14} ":a"]]]]
    ) # =>
  true

  (deep=
    #
    (peg/match cg-capture-ast "#^{:a true} [:a]")
    #
    @[[:metadata {:start 0 :end 16}
       [:deprecated-metadata-entry {:start 0 :end 11}
        [:map {:start 2 :end 11}
         [:keyword {:start 3 :end 5} ":a"]
         [:whitespace {:start 5 :end 6} " "]
         [:symbol {:start 6 :end 10} "true"]]]
       [:whitespace {:start 11 :end 12} " "]
       [:vector {:start 12 :end 16}
        [:keyword {:start 13 :end 15} ":a"]]]]
    ) # =>
  true

  (deep=
    #
    (peg/match cg-capture-ast "`a")
    #
    @[[:backtick {:start 0 :end 2}
       [:symbol {:start 1 :end 2} "a"]]]
    ) # =>
  true

  (deep=
    #
    (peg/match cg-capture-ast "'a")
    #
    @[[:quote {:start 0 :end 2}
       [:symbol {:start 1 :end 2} "a"]]]
    ) # =>
  true

  (deep=
    #
    (peg/match cg-capture-ast "~a")
    #
    @[[:unquote {:start 0 :end 2}
       [:symbol {:start 1 :end 2} "a"]]]
    ) # =>
  true

  (deep=
    #
    (peg/match cg-capture-ast "~@a")
    #
    @[[:unquote-splicing {:start 0 :end 3}
       [:symbol {:start 2 :end 3} "a"]]]
    ) # =>
  true

  (deep=
    #
    (peg/match cg-capture-ast "@a")
    #
    @[[:deref {:start 0 :end 2}
       [:symbol {:start 1 :end 2} "a"]]]
    ) # =>
  true

  (deep=
    #
    (peg/match cg-capture-ast "#(inc %)")
    #
    @[[:fn {:start 0 :end 8}
       [:list {:start 1 :end 8}
        [:symbol {:start 2 :end 5} "inc"]
        [:whitespace {:start 5 :end 6} " "]
        [:symbol {:start 6 :end 7} "%"]]]]
    ) # =>
  true

  (peg/match cg-capture-ast "#\".\"")
  # =>
  @[[:regex {:start 0 :end 4} "\".\""]]

  (deep=
    #
    (peg/match cg-capture-ast "#{:a}")
    #
    @[[:set {:start 0 :end 5}
       [:keyword {:start 2 :end 4} ":a"]]]
    ) # =>
  true

  (deep=
    #
    (peg/match cg-capture-ast "#'a")
    #
    @[[:var-quote {:start 0 :end 3}
       [:symbol {:start 2 :end 3} "a"]]]
    ) # =>
  true

  (deep=
    #
    (peg/match cg-capture-ast "#_ a")
    #
    @[[:discard {:start 0 :end 4}
       [:whitespace {:start 2 :end 3} " "]
       [:symbol {:start 3 :end 4} "a"]]]
    ) # =>
  true

  (deep=
    #
    (peg/match cg-capture-ast
               "#uuid \"00000000-0000-0000-0000-000000000000\"")
    #
    @[[:tag {:start 0 :end 44}
       [:symbol {:start 1 :end 5} "uuid"]
       [:whitespace {:start 5 :end 6} " "]
       [:string {:start 6 :end 44}
        "\"00000000-0000-0000-0000-000000000000\""]]]
    ) # =>
  true

  (peg/match cg-capture-ast " ")
  # =>
  @[[:whitespace {:start 0 :end 1} " "]]

  (peg/match cg-capture-ast "; hey")
  # =>
  @[[:comment {:start 0 :end 5} "; hey"]]

  (peg/match cg-capture-ast "#! foo")
  # =>
  @[[:comment {:start 0 :end 6} "#! foo"]]

  (deep=
    #
    (peg/match cg-capture-ast "#?(:clj 0 :cljr 1)")
    #
    @[[:conditional {:start 0 :end 18}
       [:list {:start 2 :end 18}
        [:keyword {:start 3 :end 7} ":clj"]
        [:whitespace {:start 7 :end 8} " "]
        [:number {:start 8 :end 9} "0"]
        [:whitespace {:start 9 :end 10} " "]
        [:keyword {:start 10 :end 15} ":cljr"]
        [:whitespace {:start 15 :end 16} " "]
        [:number {:start 16 :end 17} "1"]]]]
    ) # =>
  true

  (deep=
    #
    (peg/match cg-capture-ast "#?@(:clj [0 1] :cljr [1 2])")
    #
    @[[:conditional-splicing {:start 0 :end 27}
       [:list {:start 3 :end 27}
        [:keyword {:start 4 :end 8} ":clj"]
        [:whitespace {:start 8 :end 9} " "]
        [:vector {:start 9 :end 14}
         [:number {:start 10 :end 11} "0"]
         [:whitespace {:start 11 :end 12} " "]
         [:number {:start 12 :end 13} "1"]]
        [:whitespace {:start 14 :end 15} " "]
        [:keyword {:start 15 :end 20} ":cljr"]
        [:whitespace {:start 20 :end 21} " "]
        [:vector {:start 21 :end 26}
         [:number {:start 22 :end 23} "1"]
         [:whitespace {:start 23 :end 24} " "]
         [:number {:start 24 :end 25} "2"]]]]]
    ) # =>
  true

  (peg/match cg-capture-ast "##NaN")
  # =>
  @[[:symbolic {:start 0 :end 5} "NaN"]]

  (deep=
    #
    (peg/match cg-capture-ast "#=a")
    #
    @[[:eval {:start 0 :end 3}
       [:symbol {:start 2 :end 3} "a"]]]
    ) # =>
  true

  )

(defn par
  [src]
  (array/insert
    (peg/match cg-capture-ast src)
    0 :code))

(comment

  (deep=
    #
    (par "(+ 1 1)")
    #
    '@[:code
       (:list {:start 0 :end 7}
              (:symbol {:start 1 :end 2} "+")
              (:whitespace {:start 2 :end 3} " ")
              (:number {:start 3 :end 4} "1")
              (:whitespace {:start 4 :end 5} " ")
              (:number {:start 5 :end 6} "1"))]
    ) # =>
  true

  )

(defn gen*
  [ast buf]
  (case (first ast)
    :code
    (each elt (drop 1 ast)
          (gen* elt buf))
    #
    :character
    (buffer/push-string buf (in ast 2))
    :comment
    (buffer/push-string buf (in ast 2))
    :keyword
    (buffer/push-string buf (in ast 2))
    :macro-keyword
    (buffer/push-string buf (in ast 2))
    :number
    (buffer/push-string buf (in ast 2))
    :string
    (buffer/push-string buf (in ast 2))
    :symbol
    (buffer/push-string buf (in ast 2))
    :whitespace
    (buffer/push-string buf (in ast 2))
    #
    :list
    (do
      (buffer/push-string buf "(")
      (each elt (drop 2 ast)
            (gen* elt buf))
      (buffer/push-string buf ")"))
    :map
    (do
      (buffer/push-string buf "{")
      (each elt (drop 2 ast)
            (gen* elt buf))
      (buffer/push-string buf "}"))
    :set
    (do
      (buffer/push-string buf "#{")
      (each elt (drop 2 ast)
            (gen* elt buf))
      (buffer/push-string buf "}"))
    :vector
    (do
      (buffer/push-string buf "[")
      (each elt (drop 2 ast)
            (gen* elt buf))
      (buffer/push-string buf "]"))
    #
    :namespaced-map
    (do
      (buffer/push-string buf "#")
      (each elt (drop 2 ast)
            (gen* elt buf)))
    :quote
    (do
      (buffer/push-string buf "'")
      (each elt (drop 2 ast)
            (gen* elt buf)))
    :fn
    (do
      (buffer/push-string buf "#")
      (each elt (drop 2 ast)
            (gen* elt buf)))
    :deref
    (do
      (buffer/push-string buf "@")
      (each elt (drop 2 ast)
            (gen* elt buf)))
    :backtick
    (do
      (buffer/push-string buf "`")
      (each elt (drop 2 ast)
            (gen* elt buf)))
    :unquote
    (do
      (buffer/push-string buf "~")
      (each elt (drop 2 ast)
            (gen* elt buf)))
    :unquote-splicing
    (do
      (buffer/push-string buf "~@")
      (each elt (drop 2 ast)
            (gen* elt buf)))
    :discard
    (do
      (buffer/push-string buf "#_")
      (each elt (drop 2 ast)
            (gen* elt buf)))
    :var-quote
    (do
      (buffer/push-string buf "#'")
      (each elt (drop 2 ast)
            (gen* elt buf)))
    :tag
    (do
      (buffer/push-string buf "#")
      (each elt (drop 2 ast)
            (gen* elt buf)))
    :conditional
    (do
      (buffer/push-string buf "#?")
      (each elt (drop 2 ast)
            (gen* elt buf)))
    :conditional-splicing
    (do
      (buffer/push-string buf "#?@")
      (each elt (drop 2 ast)
            (gen* elt buf)))
    :eval
    (do
      (buffer/push-string buf "#=")
      (each elt (drop 2 ast)
            (gen* elt buf)))
    #
    :metadata
    (do
      (each elt (tuple/slice ast 2 -2)
            (gen* elt buf))
      (gen* (last ast) buf))
    #
    :metadata-entry
    (each elt (drop 2 ast)
          (buffer/push-string buf "^")
          (gen* elt buf))
    :deprecated-metadata-entry
    (each elt (drop 2 ast)
          (buffer/push-string buf "#^")
          (gen* elt buf))
    #
    :regex
    (do
      (buffer/push-string buf "#")
      (buffer/push-string buf (in ast 2)))
    :symbolic
    (do
      (buffer/push-string buf "##")
      (buffer/push-string buf (in ast 2)))
    #
    :auto-resolve
    (buffer/push-string buf "::")
    ))

(defn gen
  [ast]
  (let [buf @""]
    (gen* ast buf)
    (string buf)))

(comment

  (gen [:code
        [:keyword {:start 0 :end 2} ":a"]])
  # =>
  ":a"

  (gen [:code
        [:number {:start 0 :end 1} "1"]])
  # =>
  "1"

  (gen [:code
        [:whitespace {:start 0 :end 1} " "]])
  # =>
  " "

  (gen [:code
        [:list {:start 0 :end 5}
         [:number {:start 1 :end 2} "1"]
         [:whitespace {:start 2 :end 3} " "]
         [:number {:start 3 :end 4} "2"]]])
  # =>
  "(1 2)"

  (gen [:code
        [:map {:start 0 :end 6}
         [:keyword {:start 1 :end 3} ":a"]
         [:whitespace {:start 3 :end 4} " "]
         [:number {:start 4 :end 5} "1"]]])
  # =>
  "{:a 1}"

  (gen [:code
        [:vector {:start 0 :end 5}
         [:number {:start 1 :end 2} "1"]
         [:whitespace {:start 2 :end 3} " "]
         [:number {:start 3 :end 4} "2"]]])
  # =>
  "[1 2]"

  (gen [:code
        [:set {:start 0 :end 6}
         [:number {:start 2 :end 3} "1"]
         [:whitespace {:start 3 :end 4} " "]
         [:number {:start 4 :end 5} "2"]]])
  # =>
  "#{1 2}"

  (gen [:code
        [:character {:start 0 :end 8} "\\newline"]])
  # =>
  "\\newline"

  (gen [:code
        [:comment {:start 0 :end 5} ";; hi"]])
  # =>
  ";; hi"

  (gen [:code
        [:string {:start 0 :end 7} "\"smile\""]])
  # =>
  "\"smile\""

  (gen [:code
        [:symbol {:start 0 :end 1} "a"]])
  # =>
  "a"

  (gen [:code
        [:regex {:start 0 :end 4} "\".\""]])
  # =>
  "#\".\""

  (gen [:code
        [:quote {:start 0 :end 2}
         [:symbol {:start 1 :end 2} "a"]]])
  # =>
  "'a"

  (gen [:code
        [:quote {:start 0 :end 5}
         [:list {:start 1 :end 5}
          [:keyword {:start 2 :end 4} ":a"]]]])
  # =>
  "'(:a)"

  (gen [:code
        [:fn {:start 0 :end 8}
         [:list {:start 1 :end 8}
          [:symbol {:start 2 :end 5} "inc"]
          [:whitespace {:start 5 :end 6} " "]
          [:symbol {:start 6 :end 7} "%"]]]])
  # =>
  "#(inc %)"

  (gen [:code
        [:deref {:start 0 :end 2}
         [:symbol {:start 1 :end 2} "a"]]])
  # =>
  "@a"

  (gen [:code
        [:deref {:start 0 :end 11}
         [:list {:start 1 :end 11}
          [:symbol {:start 2 :end 6} "atom"]
          [:whitespace {:start 6 :end 7} " "]
          [:symbol {:start 7 :end 10} "nil"]]]])
  # =>
  "@(atom nil)"

  (gen [:code
        [:backtick {:start 0 :end 2}
         [:symbol {:start 1 :end 2} "a"]]])
  # =>
  "`a"

  (gen [:code
        [:unquote {:start 0 :end 2}
         [:symbol {:start 1 :end 2} "a"]]])
  # =>
  "~a"

  (gen [:code
        [:unquote-splicing  {:start 0 :end 3}
         [:symbol  {:start 2 :end 3} "a"]]])
  # =>
  "~@a"

  (gen [:code
        [:discard {:start 0 :end 4}
         [:whitespace {:start 2 :end 3} " "]
         [:symbol {:start 3 :end 4} "a"]]])
  # =>
  "#_ a"

  (gen [:code
        [:var-quote {:start 0 :end 3}
         [:symbol {:start 2 :end 3} "a"]]])
  # =>
  "#'a"

  (gen [:code
        [:tag {:start 0 :end 44}
         [:symbol {:start 1 :end 5} "uuid"]
         [:whitespace {:start 5 :end 6} " "]
         [:string {:start 6 :end 44}
          "\"00000000-0000-0000-0000-000000000000\""]]])
  # =>
  "#uuid \"00000000-0000-0000-0000-000000000000\""

  (gen
    [:code
     [:metadata {:start 0 :end 15}
      [:metadata-entry {:start 0 :end 10}
       [:map {:start 1 :end 10}
        [:keyword {:start 2 :end 4} ":a"]
        [:whitespace {:start 4 :end 5} " "]
        [:symbol {:start 5 :end 9} "true"]]]
      [:whitespace {:start 10 :end 11} " "]
      [:vector {:start 11 :end 15}
       [:keyword {:start 12 :end 14} ":a"]]]])
  # =>
  "^{:a true} [:a]"

  (gen
    [:code
     [:metadata {:start 0 :end 16}
      [:deprecated-metadata-entry
       {:start 0 :end 11}
       [:map {:start 2 :end 11}
        [:keyword {:start 3 :end 5} ":a"]
        [:whitespace {:start 5 :end 6} " "]
        [:symbol {:start 6 :end 10} "true"]]]
      [:whitespace {:start 11 :end 12} " "]
      [:vector {:start 12 :end 16}
       [:keyword {:start 13 :end 15} ":a"]]]])
  # =>
  "#^{:a true} [:a]"

  (gen [:code
        [:namespaced-map {:start 0 :end 6}
         [:macro-keyword {:start 1 :end 4} "::a"]
         [:map {:start 4 :end 6}]]])
  # =>
  "#::a{}"

  (gen [:code
        [:namespaced-map {:start 0 :end 5}
         [:auto-resolve {:start 1 :end 3}]
         [:map {:start 3 :end 5}]]])
  # =>
  "#::{}"

  (gen [:code
        [:namespaced-map {:start 0 :end 5}
         [:keyword  {:start 1 :end 3} ":a"]
         [:map {:start 3 :end 5}]]])
  # =>
  "#:a{}"

  (gen [:code
        [:macro-keyword {:start 0 :end 3} "::a"]])
  # =>
  "::a"

  (gen [:code
        [:symbolic {:start 0 :end 5} "Inf"]])
  # =>
  "##Inf"

  (gen [:code
        [:conditional {:start 0 :end 18}
         [:list {:start 2 :end 18}
          [:keyword {:start 3 :end 7} ":clj"]
          [:whitespace {:start 7 :end 8} " "]
          [:number {:start 8 :end 9} "0"]
          [:whitespace {:start 9 :end 10} " "]
          [:keyword {:start 10 :end 15} ":cljr"]
          [:whitespace {:start 15 :end 16} " "]
          [:number {:start 16 :end 17} "1"]]]])
  # =>
  "#?(:clj 0 :cljr 1)"

  (gen
    [:code
     [:conditional-splicing
      {:start 0 :end 27}
      [:list {:start 3 :end 27}
       [:keyword {:start 4 :end 8} ":clj"]
       [:whitespace {:start 8 :end 9} " "]
       [:vector {:start 9 :end 14}
        [:number {:start 10 :end 11} "0"]
        [:whitespace {:start 11 :end 12} " "]
        [:number {:start 12 :end 13} "1"]]
       [:whitespace {:start 14 :end 15} " "]
       [:keyword {:start 15 :end 20} ":cljr"]
       [:whitespace {:start 20 :end 21} " "]
       [:vector {:start 21 :end 26}
        [:number {:start 22 :end 23} "8"]
        [:whitespace {:start 23 :end 24} " "]
        [:number {:start 24 :end 25} "9"]]]]])
  # =>
  "#?@(:clj [0 1] :cljr [8 9])"

  (gen [:code
        [:eval {:start 0 :end 3}
         [:symbol {:start 1 :end 3} "a"]]])
  # =>
  "#=a"

  (gen [:code
        [:eval {:start 0 :end 9}
         [:list {:start 2 :end 9}
          [:symbol {:start 3 :end 4} "+"]
          [:whitespace {:start 4 :end 5} " "]
          [:symbol {:start 5 :end 6} "a"]
          [:whitespace {:start 6 :end 7} " "]
          [:symbol {:start 7 :end 8} "b"]]]])
  # =>
  "#=(+ a b)"

  )

(comment

  (comment

    (let [src (slurp (string (os/getenv "HOME")
                       "/src/clojure/src/clj/clojure/core.clj"))]
      (= (string src)
        (gen (par src))))

    )

  )
