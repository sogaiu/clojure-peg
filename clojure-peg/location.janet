(import ./grammar :prefix "")

# bl - begin line
# bc - begin column
# el - end line
# ec - end column
(defn make-attrs
  [& items]
  (zipcoll [:bl :bc :el :ec]
           items))

# this looks complicated, but it's just wrapping pieces of the original
# grammar with appropriate capture constructs that also capture start and
# end positions
(def cg-capture-ast
  # cg is a struct, need something mutable
  (let [ca (table ;(kvs cg))]
    (each kwd [:character :comment :keyword :macro-keyword :number
               :string :symbol :whitespace]
          (put ca kwd
               ~(cmt (capture (sequence (line) (column)
                                        ,(in ca kwd)
                                        (line) (column)))
                     ,|[kwd (make-attrs ;(slice $& 0 -2))
                        (last $&)])))
    (each kwd [:backtick :conditional :conditional-splicing
               :deprecated-metadata-entry :deref :discard
               :eval :metadata :metadata-entry :namespaced-map
               :quote :tag :unquote :unquote-splicing :var-quote]
          (put ca kwd
               ~(cmt (capture (sequence (line) (column)
                                        ,(in ca kwd)
                                        (line) (column)))
                     ,|[kwd (make-attrs ;(slice $& 0 2) ;(slice $& -4 -2))
                        ;(slice $& 2 -4)])))
    (each kwd [:list :map :set :vector]
          (let [open-delim (get-in ca [kwd 1])
                wrap-target (get-in ca [kwd 2])
                close-delim (get-in ca [kwd 3 1])
                rest (get-in ca [kwd 3 2])]
            (put ca kwd
                 ~(cmt (capture (sequence (line) (column)
                                          ,open-delim
                                          ,wrap-target
                                          (choice ,close-delim ,rest)
                                          (line) (column)))
                       ,|[kwd (make-attrs ;(slice $& 0 2) ;(slice $& -4 -2))
                          ;(slice $& 2 -4)]))))
    #
    (put ca :regex
         ~(cmt (capture (sequence (line) (column)
                                  ,(in ca :regex)
                                  (line) (column)))
               ,|[:regex (make-attrs ;(slice $& 0 2) ;(slice $& -4 -2))
                  (last (get $& 2))]))
    #
    (put ca :fn
         ~(cmt (capture (sequence (line) (column)
                                  ,(in ca :fn)
                                  (line) (column)))
               ,|[:fn (make-attrs ;(slice $& 0 2) ;(slice $& -4 -2))
                  (get $& 2)]))
    #
    (put ca :symbolic
         ~(cmt (capture (sequence (line) (column)
                                  ,(in ca :symbolic)
                                  (line) (column)))
               ,|[:symbolic (make-attrs ;(slice $& 0 2) ;(slice $& -4 -2))
                            (last ;(slice $& 2 -4))]))
    #
    (put ca :auto-resolve
         ~(cmt (capture (sequence (line) (column)
                                  ,(in ca :auto-resolve)
                                  (line) (column)))
               ,|[:auto-resolve (make-attrs ;(slice $& 0 -2)) ]))
    # tried using a table with a peg but had a problem, so use a struct
    (table/to-struct ca)))

(comment

  (peg/match cg-capture-ast ":a")
  # =>
  @[[:keyword @{:bc 1 :bl 1 :ec 3 :el 1} ":a"]]

  (peg/match cg-capture-ast "\"smile\"")
  # =>
  @[[:string @{:bc 1 :bl 1 :ec 8 :el 1} "\"smile\""]]

  (peg/match cg-capture-ast "1/2")
  # =>
  @[[:number @{:bc 1 :bl 1 :ec 4 :el 1} "1/2"]]

  (peg/match cg-capture-ast "defmacro")
  # =>
  @[[:symbol @{:bc 1 :bl 1 :ec 9 :el 1} "defmacro"]]

  (peg/match cg-capture-ast "::a")
  # =>
  @[[:macro-keyword @{:bc 1 :bl 1 :ec 4 :el 1} "::a"]]

  (peg/match cg-capture-ast "\\a")
  # =>
  @[[:character @{:bc 1 :bl 1 :ec 3 :el 1} "\\a"]]

  (peg/match cg-capture-ast "{}")
  # =>
  @[[:map @{:bc 1 :bl 1 :ec 3 :el 1}]]

  (deep=
    #
    (peg/match cg-capture-ast "{:a 1}")
    #
    @[[:map @{:bc 1 :bl 1 :ec 7 :el 1}
       [:keyword @{:bc 2 :bl 1 :ec 4 :el 1} ":a"]
       [:whitespace @{:bc 4 :bl 1 :ec 5 :el 1} " "]
       [:number @{:bc 5 :bl 1 :ec 6 :el 1} "1"]]])
  # =>
  true

  (deep=
    #
    (peg/match cg-capture-ast "#::{}")
    #
    @[[:namespaced-map @{:bc 1 :bl 1 :ec 6 :el 1}
       [:auto-resolve @{:bc 2 :bl 1 :ec 4 :el 1}]
       [:map @{:bc 4 :bl 1 :ec 6 :el 1}]]])
  # =>
  true

  (deep=
    #
    (peg/match cg-capture-ast "#::a{}")
    #
    @[[:namespaced-map @{:bc 1 :bl 1 :ec 7 :el 1}
       [:macro-keyword @{:bc 2 :bl 1 :ec 5 :el 1} "::a"]
       [:map @{:bc 5 :bl 1 :ec 7 :el 1}]]])
  # =>
  true

  (deep=
    #
    (peg/match cg-capture-ast "#:a{}")
    #
    @[[:namespaced-map @{:bc 1 :bl 1 :ec 6 :el 1}
       [:keyword @{:bc 2 :bl 1 :ec 4 :el 1} ":a"]
       [:map @{:bc 4 :bl 1 :ec 6 :el 1}]]])
  # =>
  true

  (peg/match cg-capture-ast "[]")
  # =>
  @[[:vector @{:bc 1 :bl 1 :ec 3 :el 1}]]

  (deep=
    #
    (peg/match cg-capture-ast "[:a]")
    #
    @[[:vector @{:bc 1 :bl 1 :ec 5 :el 1}
       [:keyword @{:bc 2 :bl 1 :ec 4 :el 1} ":a"]]])
  # =>
  true

  (peg/match cg-capture-ast "()")
  # =>
  @[[:list @{:bc 1 :bl 1 :ec 3 :el 1}]]

  (deep=
    #
    (peg/match cg-capture-ast "(:a)")
    #
    @[[:list @{:bc 1 :bl 1 :ec 5 :el 1}
       [:keyword @{:bc 2 :bl 1 :ec 4 :el 1} ":a"]]]
    )
  # =>
  true

  (deep=
    #
    (peg/match cg-capture-ast "^{:a true} [:a]")
    #
    @[[:metadata @{:bc 1 :bl 1 :ec 16 :el 1}
       [:metadata-entry @{:bc 1 :bl 1 :ec 11 :el 1}
        [:map @{:bc 2 :bl 1 :ec 11 :el 1}
         [:keyword @{:bc 3 :bl 1 :ec 5 :el 1} ":a"]
         [:whitespace @{:bc 5 :bl 1 :ec 6 :el 1} " "]
         [:symbol @{:bc 6 :bl 1 :ec 10 :el 1} "true"]]]
       [:whitespace @{:bc 11 :bl 1 :ec 12 :el 1} " "]
       [:vector @{:bc 12 :bl 1 :ec 16 :el 1}
        [:keyword @{:bc 13 :bl 1 :ec 15 :el 1} ":a"]]]]
    )
  # =>
  true

  (deep=
    #
    (peg/match cg-capture-ast "#^{:a true} [:a]")
    #
    @[[:metadata @{:bc 1 :bl 1 :ec 17 :el 1}
       [:deprecated-metadata-entry @{:bc 1 :bl 1 :ec 12 :el 1}
        [:map @{:bc 3 :bl 1 :ec 12 :el 1}
         [:keyword @{:bc 4 :bl 1 :ec 6 :el 1} ":a"]
         [:whitespace @{:bc 6 :bl 1 :ec 7 :el 1} " "]
         [:symbol @{:bc 7 :bl 1 :ec 11 :el 1} "true"]]]
       [:whitespace @{:bc 12 :bl 1 :ec 13 :el 1} " "]
       [:vector @{:bc 13 :bl 1 :ec 17 :el 1}
        [:keyword @{:bc 14 :bl 1 :ec 16 :el 1} ":a"]]]]
    )
  # =>
  true

  (deep=
    #
    (peg/match cg-capture-ast "`a")
    #
    @[[:backtick @{:bc 1 :bl 1 :ec 3 :el 1}
       [:symbol @{:bc 2 :bl 1 :ec 3 :el 1} "a"]]]
    )
  # =>
  true

  (deep=
    #
    (peg/match cg-capture-ast "'a")
    #
    @[[:quote @{:bc 1 :bl 1 :ec 3 :el 1}
       [:symbol @{:bc 2 :bl 1 :ec 3 :el 1} "a"]]]
    )
  # =>
  true

  (deep=
    #
    (peg/match cg-capture-ast "~a")
    #
    @[[:unquote @{:bc 1 :bl 1 :ec 3 :el 1}
       [:symbol @{:bc 2 :bl 1 :ec 3 :el 1} "a"]]]
    )
  # =>
  true

  (deep=
    #
    (peg/match cg-capture-ast "~@a")
    #
    @[[:unquote-splicing @{:bc 1 :bl 1 :ec 4 :el 1}
       [:symbol @{:bc 3 :bl 1 :ec 4 :el 1} "a"]]]
    )
  # =>
  true

  (deep=
    #
    (peg/match cg-capture-ast "@a")
    #
    @[[:deref @{:bc 1 :bl 1 :ec 3 :el 1}
       [:symbol @{:bc 2 :bl 1 :ec 3 :el 1} "a"]]]
    )
  # =>
  true

  (deep=
    #
    (peg/match cg-capture-ast "#(inc %)")
    #
    @[[:fn @{:bc 1 :bl 1 :ec 9 :el 1}
       [:list @{:bc 2 :bl 1 :ec 9 :el 1}
        [:symbol @{:bc 3 :bl 1 :ec 6 :el 1} "inc"]
        [:whitespace @{:bc 6 :bl 1 :ec 7 :el 1} " "]
        [:symbol @{:bc 7 :bl 1 :ec 8 :el 1} "%"]]]]
    )
  # =>
  true

  (peg/match cg-capture-ast "#\".\"")
  # =>
  @[[:regex @{:bc 1 :bl 1 :ec 5 :el 1} "\".\""]]

  (deep=
    #
    (peg/match cg-capture-ast "#{:a}")
    #
    @[[:set @{:bc 1 :bl 1 :ec 6 :el 1}
       [:keyword @{:bc 3 :bl 1 :ec 5 :el 1} ":a"]]]
    )
  # =>
  true

  (deep=
    #
    (peg/match cg-capture-ast "#'a")
    #
    @[[:var-quote @{:bc 1 :bl 1 :ec 4 :el 1}
       [:symbol @{:bc 3 :bl 1 :ec 4 :el 1} "a"]]]
    )
  # =>
  true

  (deep=
    #
    (peg/match cg-capture-ast "#_ a")
    #
    @[[:discard @{:bc 1 :bl 1 :ec 5 :el 1}
       [:whitespace @{:bc 3 :bl 1 :ec 4 :el 1} " "]
       [:symbol @{:bc 4 :bl 1 :ec 5 :el 1} "a"]]]
    )
  # =>
  true

  (deep=
    #
    (peg/match cg-capture-ast
               "#uuid \"00000000-0000-0000-0000-000000000000\"")
    #
    @[[:tag @{:bc 1 :bl 1 :ec 45 :el 1}
       [:symbol @{:bc 2 :bl 1 :ec 6 :el 1} "uuid"]
       [:whitespace @{:bc 6 :bl 1 :ec 7 :el 1} " "]
       [:string @{:bc 7 :bl 1 :ec 45 :el 1}
        "\"00000000-0000-0000-0000-000000000000\""]]]
    )
  # =>
  true

  (peg/match cg-capture-ast " ")
  # =>
  @[[:whitespace @{:bc 1 :bl 1 :ec 2 :el 1} " "]]

  (peg/match cg-capture-ast "; hey")
  # =>
  @[[:comment @{:bc 1 :bl 1 :ec 6 :el 1} "; hey"]]

  (peg/match cg-capture-ast "#! foo")
  # =>
  @[[:comment @{:bc 1 :bl 1 :ec 7 :el 1} "#! foo"]]

  (deep=
    #
    (peg/match cg-capture-ast "#?(:clj 0 :cljr 1)")
    #
    @[[:conditional @{:bc 1 :bl 1 :ec 19 :el 1}
       [:list @{:bc 3 :bl 1 :ec 19 :el 1}
        [:keyword @{:bc 4 :bl 1 :ec 8 :el 1} ":clj"]
        [:whitespace @{:bc 8 :bl 1 :ec 9 :el 1} " "]
        [:number @{:bc 9 :bl 1 :ec 10 :el 1} "0"]
        [:whitespace @{:bc 10 :bl 1 :ec 11 :el 1} " "]
        [:keyword @{:bc 11 :bl 1 :ec 16 :el 1} ":cljr"]
        [:whitespace @{:bc 16 :bl 1 :ec 17 :el 1} " "]
        [:number @{:bc 17 :bl 1 :ec 18 :el 1} "1"]]]]
    )
  # =>
  true

  (deep=
    #
    (peg/match cg-capture-ast "#?@(:clj [0 1] :cljr [1 2])")
    #
    @[[:conditional-splicing @{:bc 1 :bl 1 :ec 28 :el 1}
       [:list @{:bc 4 :bl 1 :ec 28 :el 1}
        [:keyword @{:bc 5 :bl 1 :ec 9 :el 1} ":clj"]
        [:whitespace @{:bc 9 :bl 1 :ec 10 :el 1} " "]
        [:vector @{:bc 10 :bl 1 :ec 15 :el 1}
         [:number @{:bc 11 :bl 1 :ec 12 :el 1} "0"]
         [:whitespace @{:bc 12 :bl 1 :ec 13 :el 1} " "]
         [:number @{:bc 13 :bl 1 :ec 14 :el 1} "1"]]
        [:whitespace @{:bc 15 :bl 1 :ec 16 :el 1} " "]
        [:keyword @{:bc 16 :bl 1 :ec 21 :el 1} ":cljr"]
        [:whitespace @{:bc 21 :bl 1 :ec 22 :el 1} " "]
        [:vector @{:bc 22 :bl 1 :ec 27 :el 1}
         [:number @{:bc 23 :bl 1 :ec 24 :el 1} "1"]
         [:whitespace @{:bc 24 :bl 1 :ec 25 :el 1} " "]
         [:number @{:bc 25 :bl 1 :ec 26 :el 1} "2"]]]]]
    )
  # =>
  true

  (peg/match cg-capture-ast "##NaN")
  # =>
  @[[:symbolic @{:bc 1 :bl 1 :ec 6 :el 1} "NaN"]]

  (deep=
    #
    (peg/match cg-capture-ast "#=a")
    #
    @[[:eval @{:bc 1 :bl 1 :ec 4 :el 1}
       [:symbol @{:bc 3 :bl 1 :ec 4 :el 1} "a"]]]
    )
  # =>
  true

  )

(def loc-top-level-ast
  (let [ltla (table ;(kvs cg-capture-ast))]
    (put ltla :main
         ~(sequence (line) (column)
                    :input
                    (line) (column)))
    (table/to-struct ltla)))

(def mod-ast
  (let [ma (table ;(kvs cg-capture-ast))]
    (put ma :main
         ~(sequence (line) (column)
                    (some :input)
                    (line) (column)))
    (table/to-struct ma)))

(defn par
  [src &opt start single]
  (default start 0)
  (if single
    (if-let [[bl bc tree el ec]
             (peg/match loc-top-level-ast src start)]
      @[:code (make-attrs bl bc el ec) tree]
      @[:code])
    (if-let [captures (peg/match mod-ast src start)]
      (let [[bl bc] (slice captures 0 2)
            [el ec] (slice captures -3)
            trees (array/slice captures 2 -3)]
        (array/insert trees 0
                      :code (make-attrs bl bc el ec)))
      @[:code])))

(comment

  (deep=
    #
    (par "(+ 1 1)")
    #
    @[:code @{:bc 1 :bl 1 :ec 8 :el 1}
      [:list @{:bc 1 :bl 1 :ec 8 :el 1}
       [:symbol @{:bc 2 :bl 1 :ec 3 :el 1} "+"]
       [:whitespace @{:bc 3 :bl 1 :ec 4 :el 1} " "]
       [:number @{:bc 4 :bl 1 :ec 5 :el 1} "1"]
       [:whitespace @{:bc 5 :bl 1 :ec 6 :el 1} " "]
       [:number @{:bc 6 :bl 1 :ec 7 :el 1} "1"]]]
    )
  # =>
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

  (gen [:code {}
        [:keyword {} ":a"]])
  # =>
  ":a"

  (gen [:code {}
        [:number {} "1"]])
  # =>
  "1"

  (gen [:code {}
        [:whitespace {} " "]])
  # =>
  " "

  (gen [:code {}
        [:list {}
         [:number {} "1"]
         [:whitespace {} " "]
         [:number {} "2"]]])
  # =>
  "(1 2)"

  (gen [:code {}
        [:map {}
         [:keyword {} ":a"]
         [:whitespace {} " "]
         [:number {} "1"]]])
  # =>
  "{:a 1}"

  (gen [:code {}
        [:vector {}
         [:number {} "1"]
         [:whitespace {} " "]
         [:number {} "2"]]])
  # =>
  "[1 2]"

  (gen [:code {}
        [:set {}
         [:number {} "1"]
         [:whitespace {} " "]
         [:number {} "2"]]])
  # =>
  "#{1 2}"

  (gen [:code {}
        [:character {} "\\newline"]])
  # =>
  "\\newline"

  (gen [:code {}
        [:comment {} ";; hi"]])
  # =>
  ";; hi"

  (gen [:code {}
        [:string {} "\"smile\""]])
  # =>
  "\"smile\""

  (gen [:code {}
        [:symbol {} "a"]])
  # =>
  "a"

  (gen [:code {}
        [:regex {} "\".\""]])
  # =>
  "#\".\""

  (gen [:code {}
        [:quote {}
         [:symbol {} "a"]]])
  # =>
  "'a"

  (gen [:code {}
        [:quote {}
         [:list {}
          [:keyword {} ":a"]]]])
  # =>
  "'(:a)"

  (gen [:code {}
        [:fn {}
         [:list {}
          [:symbol {} "inc"]
          [:whitespace {} " "]
          [:symbol {} "%"]]]])
  # =>
  "#(inc %)"

  (gen [:code {}
        [:deref {}
         [:symbol {} "a"]]])
  # =>
  "@a"

  (gen [:code {}
        [:deref {}
         [:list {}
          [:symbol {} "atom"]
          [:whitespace {} " "]
          [:symbol {} "nil"]]]])
  # =>
  "@(atom nil)"

  (gen [:code {}
        [:backtick {}
         [:symbol {} "a"]]])
  # =>
  "`a"

  (gen [:code {}
        [:unquote {}
         [:symbol {} "a"]]])
  # =>
  "~a"

  (gen [:code {}
        [:unquote-splicing {}
         [:symbol {} "a"]]])
  # =>
  "~@a"

  (gen [:code {}
        [:discard {}
         [:whitespace {} " "]
         [:symbol {} "a"]]])
  # =>
  "#_ a"

  (gen [:code {}
        [:var-quote {}
         [:symbol {} "a"]]])
  # =>
  "#'a"

  (gen [:code {}
        [:tag {}
         [:symbol {} "uuid"]
         [:whitespace {} " "]
         [:string {}
          "\"00000000-0000-0000-0000-000000000000\""]]])
  # =>
  "#uuid \"00000000-0000-0000-0000-000000000000\""

  (gen
    [:code {}
     [:metadata {}
      [:metadata-entry {}
       [:map {}
        [:keyword {} ":a"]
        [:whitespace {} " "]
        [:symbol {} "true"]]]
      [:whitespace {} " "]
      [:vector {}
       [:keyword {} ":a"]]]])
  # =>
  "^{:a true} [:a]"

  (gen
    [:code {}
     [:metadata {}
      [:deprecated-metadata-entry
       {}
       [:map {}
        [:keyword {} ":a"]
        [:whitespace {} " "]
        [:symbol {} "true"]]]
      [:whitespace {} " "]
      [:vector {}
       [:keyword {} ":a"]]]])
  # =>
  "#^{:a true} [:a]"

  (gen [:code {}
        [:namespaced-map {}
         [:macro-keyword {} "::a"]
         [:map {}]]])
  # =>
  "#::a{}"

  (gen [:code {}
        [:namespaced-map {}
         [:auto-resolve {}]
         [:map {}]]])
  # =>
  "#::{}"

  (gen [:code {}
        [:namespaced-map {}
         [:keyword  {} ":a"]
         [:map {}]]])
  # =>
  "#:a{}"

  (gen [:code {}
        [:macro-keyword {} "::a"]])
  # =>
  "::a"

  (gen [:code {}
        [:symbolic {} "Inf"]])
  # =>
  "##Inf"

  (gen [:code {}
        [:conditional {}
         [:list {}
          [:keyword {} ":clj"]
          [:whitespace {} " "]
          [:number {} "0"]
          [:whitespace {} " "]
          [:keyword {} ":cljr"]
          [:whitespace {} " "]
          [:number {} "1"]]]])
  # =>
  "#?(:clj 0 :cljr 1)"

  (gen
    [:code {}
     [:conditional-splicing
      {}
      [:list {}
       [:keyword {} ":clj"]
       [:whitespace {} " "]
       [:vector {}
        [:number {} "0"]
        [:whitespace {} " "]
        [:number {} "1"]]
       [:whitespace {} " "]
       [:keyword {} ":cljr"]
       [:whitespace {} " "]
       [:vector {}
        [:number {} "8"]
        [:whitespace {} " "]
        [:number {} "9"]]]]])
  # =>
  "#?@(:clj [0 1] :cljr [8 9])"

  (gen [:code {}
        [:eval {}
         [:symbol {} "a"]]])
  # =>
  "#=a"

  (gen [:code {}
        [:eval {}
         [:list {}
          [:symbol {} "+"]
          [:whitespace {} " "]
          [:symbol {} "a"]
          [:whitespace {} " "]
          [:symbol {} "b"]]]])
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
