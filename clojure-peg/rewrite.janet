# QUESTIONS:
#
# XXX: capturing and storing location info has a cost (see
#      tests at end of experimental.janet).  what is an appropriate way?
#
# XXX: what is a good way to store location info?  suppose just byte
#      offsets are being used, one idea is to use the second position to
#      store a struct:
#
#        [:vector {:start 23 :end 10} [:keyword ":a"] ... ]
#
#      another idea is to use the second (and possibly some subsequent
#      positions):
#
#        [:vector 23 10 [:keyword ":a"] ... ]
#
#      the former seems more extensible and readable, while the latter
#      is likely more performant.
#
# XXX: byte offset seems like a more performant thing to capture
#      and store compared to line and column information.  line and
#      column info seem like extra processing with low return.  is that
#      sufficient?

(import ./grammar :prefix "")

(def cg-capture-ast
  (->
    # cg is a struct, need something mutable
    (table ;(kvs cg))
    # override things that need to be captured
    (put :whitespace
      ~(cmt (capture
              (some (set "\f\n\r\t, ")))
            ,|[:whitespace $]))
    #
    (put :comment
      ~(cmt
         (capture
           (sequence (choice ";"
                             "#!")
                     (any (if-not (set "\r\n")
                                  1))))
         ,|[:comment $]))
    (put :keyword
      ~(cmt
         (capture
           (sequence ":"
                     (choice "/"
                             (sequence :keyword_head
                                       (any :keyword_body)))))
         ,|[:keyword $]))
    (put :macro_keyword
      ~(cmt
         (capture
           (sequence "::"
                     :keyword_head
                     (any :keyword_body)))
         ,|[:macro_keyword $]))
    (put :string
      ~(cmt
         (capture
           (sequence "\""
                     (any (if-not (set "\"\\")
                                  1))
                     (any (sequence "\\"
                                    1
                                    (any (if-not (set "\"\\")
                                                 1))))
                     "\""))
         ,|[:string $]))
    (put :number
      ~(cmt
         (capture
           (sequence (opt (set "+-"))
                     (some :digit)
                     (choice :ratio_suffix
                             :double_suffix
                             :long_suffix)))
         ,|[:number $]))
    (put :character
      ~(cmt
         (capture
           (sequence "\\"
                     (choice :named_char
                             :unicode
                             :unicode_char)))
         ,|[:character $]))
    (put :symbol
      ~(cmt
         (capture
           (sequence :name_head
                     (any :name_body)))
         ,|[:symbol $]))
    #
    (put :metadata
      ~(cmt
         (capture
           (sequence (some (sequence (choice :metadata_entry
                                             :deprecated_metadata_entry)
                                     (opt :whitespace)))
                     (choice :collection
                             :namespaced_map
                             :set
                             :tag
                             :fn
                             :unquote_splicing
                             :unquote
                             :symbol)))
         ,|[:metadata ;(slice $& 0 -2)]))
    (put :metadata_entry
      ~(cmt
         (capture
           (sequence "^"
                     (choice :map
                             :string
                             :macro_keyword
                             :keyword
                             :symbol)))
         ,|[:metadata_entry ;(slice $& 0 -2)]))
    (put :deprecated_metadata_entry
      ~(cmt
         (capture
           (sequence "#^"
                     (choice :map
                             :string
                             :macro_keyword
                             :keyword
                             :symbol)))
         ,|[:deprecated_metadata_entry ;(slice $& 0 -2)]))
    (put :backtick
      ~(cmt
         (capture
           (sequence "`"
                     (opt :whitespace)
                          :form))
         ,|[:backtick ;(slice $& 0 -2)]))
    (put :quote
      ~(cmt
         (capture
           (sequence "'"
                     (opt :whitespace)
                          :form))
         ,|[:quote ;(slice $& 0 -2)]))
    (put :unquote
      ~(cmt
         (capture
           (sequence "~"
                     (opt :whitespace)
                          :form))
         ,|[:unquote ;(slice $& 0 -2)]))
    (put :unquote_splicing
      ~(cmt
         (capture
           (sequence "~@"
                     (opt :whitespace)
                          :form))
         ,|[:unquote_splicing ;(slice $& 0 -2)]))
    (put :deref
      ~(cmt
         (capture
           (sequence "@"
                     (opt :whitespace)
                          :form))
         ,|[:deref ;(slice $& 0 -2)]))
    (put :var_quote
      ~(cmt
         (capture
           (sequence "#'"
                     (opt :whitespace)
                          :form))
            ,|[:var_quote ;(slice $& 0 -2)]))
    (put :conditional
      ~(cmt
         (capture
           (sequence "#?"
                     (opt :whitespace)
                     :list))
         ,|[:conditional ;(slice $& 0 -2)]))
    (put :conditional_splicing
      ~(cmt
         (capture
           (sequence "#?@"
                     (opt :whitespace)
                          :list))
         ,|[:conditional_splicing ;(slice $& 0 -2)]))
    (put :namespaced_map
      ~(cmt
         (capture
           (sequence "#"
                     (choice :macro_keyword
                             :auto_resolve
                             :keyword)
                     (opt :whitespace)
                          :map))
         ,|[:namespaced_map ;(slice $& 0 -2)]))
    (put :discard
      ~(cmt
         (capture
           (sequence "#_"
                     (opt (sequence (opt :whitespace)
                                    :discard))
                     (opt :whitespace)
                     :form))
         ,|[:discard ;(slice $& 0 -2)]))
    (put :tag
      ~(cmt
         (capture
           (sequence "#"
                     :symbol
                     (opt :whitespace)
                     (choice :tag
                             :collection
                             :literal)))
         ,|[:tag ;(slice $& 0 -2)]))
    (put :eval
      ~(cmt
         (capture
           (sequence "#="
                     (opt :whitespace)
                     (choice :list
                             :symbol)))
         ,|[:eval ;(slice $& 0 -2)]))
    #
    (put :regex
      ~(sequence "#"
                 (cmt (capture :string)
                      ,|[:regex (in $& 1)])))
    #
    (put :fn
      ~(sequence "#"
                 (cmt (capture :list)
                      ,|[:fn ;(slice $& 0 -2)])))
    #
    (put :symbolic
      ~(cmt
         (capture
           (sequence "##"
                     :symbol))
         ,|[:symbolic (in ;(slice $& 0 -2) 1)]))
    #
    (put :auto_resolve
      ~(cmt (capture "::")
            ,(fn [_] [:auto_resolve])))
    #
    (put :list
      ~(sequence "("
                 (cmt (capture (any :input))
                      ,|[:list ;(slice $& 0 -2)])
                 (choice ")" (error ""))))
    (put :vector
      ~(sequence "["
                 (cmt (capture (any :input))
                      ,|[:vector ;(slice $& 0 -2)])
                 (choice "]" (error ""))))
    (put :map
      ~(sequence "{"
                 (cmt (capture (any :input))
                      ,|[:map ;(slice $& 0 -2)])
                 (choice "}" (error ""))))
    (put :set
      ~(sequence "#{"
                 (cmt (capture (any :input))
                      ,|[:set ;(slice $& 0 -2)])
                 (choice "}" (error ""))))
    # tried using a table with a peg but had a problem, so use a struct
    table/to-struct))

(comment

  (peg/match cg-capture-ast " ")
  # => @[[:whitespace " "]]

  (peg/match cg-capture-ast "; hello")
  # => @[[:comment "; hello"]]

  (peg/match cg-capture-ast "a")
  # => @[[:symbol "a"]]

  (peg/match cg-capture-ast ":a")
  # => @[[:keyword ":a"]]

  (peg/match cg-capture-ast "(:a :b :c)")
  ``
  @[[:list
     [:keyword ":a"] [:whitespace " "]
     [:keyword ":b"] [:whitespace " "]
     [:keyword ":c"]]]
  ``

  (peg/match cg-capture-ast "[:a :b :c]")
  ``
  @[[:vector
     [:keyword ":a"] [:whitespace " "]
     [:keyword ":b"] [:whitespace " "]
     [:keyword ":c"]]]
  ``

  (peg/match cg-capture-ast "{:a 1 :b 2}")
  ``
  @[[:map
     [:keyword ":a"] [:whitespace " "]
     [:number "1"] [:whitespace " "]
     [:keyword ":b"] [:whitespace " "]
     [:number "2"]]]
  ``

  (peg/match cg-capture-ast "#{:a :b :c}")
  ``
  @[[:set
     [:keyword ":a"] [:whitespace " "]
     [:keyword ":b"] [:whitespace " "]
     [:keyword ":c"]]]
  ``

  (peg/match cg-capture-ast "\"a\"")
  # => @[[:string "\"a\""]]

  (peg/match cg-capture-ast "#\".\"")
  # => @[[:regex "\".\""]]

  (peg/match cg-capture-ast "#(inc %)")
  ``
  @[[:fn
     [:list
      [:symbol "inc"] [:whitespace " "]
      [:symbol "%"]]]]
  ``

  (peg/match cg-capture-ast "#::a{}")
  # => @[[:namespaced_map [:macro_keyword "::a"] [:map]]]

  (peg/match cg-capture-ast "#::{}")
  # => @[[:namespaced_map [:auto_resolve] [:map]]]

  (peg/match cg-capture-ast "#:a{}")
  # => @[[:namespaced_map [:keyword ":a"] [:map]]]

  (peg/match cg-capture-ast "#=a")
  # => @[[:eval [:symbol "a"]]]

  (peg/match cg-capture-ast "#= a")
  # => @[[:eval [:whitespace " "] [:symbol "a"]]]

  (peg/match cg-capture-ast "#=(+ a b)")
  ``
  @[[:eval
     [:list
      [:symbol "+"] [:whitespace " "]
      [:symbol "a"] [:whitespace " "]
      [:symbol "b"]]]]
  ``

  (peg/match cg-capture-ast "##Inf")
  # => @[[:symbolic "Inf"]]

  (peg/match cg-capture-ast "#'a")
  # => @[[:var_quote [:symbol "a"]]]

  (peg/match cg-capture-ast "\\newline")
  # => @[[:character "\\newline"]]

  (peg/match cg-capture-ast "\\ua08e")
  # => @[[:character "\\ua08e"]]

  (peg/match cg-capture-ast "\\a")
  # => @[[:character "\\a"]]

  (peg/match cg-capture-ast "@a")
  # => @[[:deref [:symbol "a"]]]

  (peg/match cg-capture-ast "'a")
  # => @[[:quote [:symbol "a"]]]

  (peg/match cg-capture-ast "`a")
  # => @[[:backtick [:symbol "a"]]]

  (peg/match cg-capture-ast "~a")
  # => @[[:unquote [:symbol "a"]]]

  (peg/match cg-capture-ast "~(:a :b :c)")
  ``
  @[[:unquote
     [:list
      [:keyword ":a"] [:whitespace " "]
      [:keyword ":b"] [:whitespace " "]
      [:keyword ":c"]]]]
  ``

  (peg/match cg-capture-ast "~@(:a :b :c)")
  ``
  @[[:unquote_splicing
     [:list
      [:keyword ":a"] [:whitespace " "]
      [:keyword ":b"] [:whitespace " "]
      [:keyword ":c"]]]]
  ``

  (peg/match cg-capture-ast "1")
  # => @[[:number "1"]]

  (peg/match cg-capture-ast "^{:a true} [:a]")
  ``
  @[[:metadata
     [:metadata_entry
      [:map
       [:keyword ":a"] [:whitespace " "]
       [:symbol "true"]]]
     [:whitespace " "]
     [:vector
      [:keyword ":a"]]]]
  ``

  (peg/match cg-capture-ast "#^{:a true} [:a]")
  ``
  @[[:metadata
     [:deprecated_metadata_entry
      [:map
       [:keyword ":a"] [:whitespace " "]
       [:symbol "true"]]]
     [:whitespace " "]
     [:vector
      [:keyword ":a"]]]]
  ``

  (peg/match cg-capture-ast "#uuid \"00000000-0000-0000-0000-000000000000\"")
  ``
  @[[:tag
     [:symbol "uuid"] [:whitespace " "]
     [:string "\"00000000-0000-0000-0000-000000000000\""]]]
  ``

  (peg/match cg-capture-ast "#?(:clj 0 :cljr 1)")
  ``
  @[[:conditional
     [:list
      [:keyword ":clj"] [:whitespace " "]
      [:number "0"] [:whitespace " "]
      [:keyword ":cljr"] [:whitespace " "]
      [:number "1"]]]]
  ``

  (peg/match cg-capture-ast "#?@(:clj [0 1] :cljr [8 9])")
  ``
  @[[:conditional_splicing
     [:list
      [:keyword ":clj"] [:whitespace " "]
      [:vector
       [:number "0"] [:whitespace " "]
       [:number "1"]] [:whitespace " "]
      [:keyword ":cljr"] [:whitespace " "]
      [:vector
       [:number "8"] [:whitespace " "]
       [:number "9"]]]]]
  ``

  (peg/match cg-capture-ast "#_ a")
  # => @[[:discard [:whitespace " "] [:symbol "a"]]]

  (peg/match cg-capture-ast "#_ #_ :a :b")
  `` @[[:discard
        [:whitespace " "]
        [:discard
         [:whitespace " "] [:keyword ":a"]]
        [:whitespace " "]
        [:keyword ":b"]]]
  ``

  )

(defn ast
  [src]
  (array/insert
    (peg/match cg-capture-ast src)
    0 :code))

(comment

  (ast "(+ 1 1)")
  ``
  '@[:code
     (:list
      (:symbol "+") (:whitespace " ")
      (:number "1") (:whitespace " ")
      (:number "1"))]
  ``

  )

(defn code*
  [ast buf]
  (case (first ast)
    :code
    (each elt (drop 1 ast)
          (code* elt buf))
    #
    :list
    (do
      (buffer/push-string buf "(")
      (each elt (drop 1 ast)
            (code* elt buf))
      (buffer/push-string buf ")"))
    #
    :vector
    (do
      (buffer/push-string buf "[")
      (each elt (drop 1 ast)
            (code* elt buf))
      (buffer/push-string buf "]"))
    #
    :map
    (do
      (buffer/push-string buf "{")
      (each elt (drop 1 ast)
            (code* elt buf))
      (buffer/push-string buf "}"))
    #
    :set
    (do
      (buffer/push-string buf "#{")
      (each elt (drop 1 ast)
            (code* elt buf))
      (buffer/push-string buf "}"))
    #
    :namespaced_map
    (do
      (buffer/push-string buf "#")
      (each elt (drop 1 ast)
            (code* elt buf)))
    #
    :character
    (buffer/push-string buf (in ast 1))
    #
    :comment
    (buffer/push-string buf (in ast 1))
    #
    :keyword
    (buffer/push-string buf (in ast 1))
    #
    :macro_keyword
    (buffer/push-string buf (in ast 1))
    #
    :number
    (buffer/push-string buf (in ast 1))
    #
    :string
    (buffer/push-string buf (in ast 1))
    #
    :symbol
    (buffer/push-string buf (in ast 1))
    #
    :whitespace
    (buffer/push-string buf (in ast 1))
    #
    :regex
    (do
      (buffer/push-string buf "#")
      (buffer/push-string buf (in ast 1)))
    #
    :quote
    (do
      (buffer/push-string buf "'")
      (each elt (drop 1 ast)
            (code* elt buf)))
    #
    :fn
    (do
      (buffer/push-string buf "#")
      (each elt (drop 1 ast)
            (code* elt buf)))
    #
    :deref
    (do
      (buffer/push-string buf "@")
      (each elt (drop 1 ast)
            (code* elt buf)))
    #
    :backtick
    (do
      (buffer/push-string buf "`")
      (each elt (drop 1 ast)
            (code* elt buf)))
    #
    :unquote
    (do
      (buffer/push-string buf "~")
      (each elt (drop 1 ast)
            (code* elt buf)))
    #
    :unquote_splicing
    (do
      (buffer/push-string buf "~@")
      (each elt (drop 1 ast)
            (code* elt buf)))
    #
    :discard
    (do
      (buffer/push-string buf "#_")
      (each elt (drop 1 ast)
            (code* elt buf)))
    #
    :var_quote
    (do
      (buffer/push-string buf "#'")
      (each elt (drop 1 ast)
            (code* elt buf)))
    #
    :tag
    (do
      (buffer/push-string buf "#")
      (each elt (drop 1 ast)
            (code* elt buf)))
    #
    :metadata_entry
    (each elt (drop 1 ast)
          (buffer/push-string buf "^")
          (code* elt buf))
    #
    :deprecated_metadata_entry
    (each elt (drop 1 ast)
          (buffer/push-string buf "#^")
          (code* elt buf))
    #
    :metadata
    (do
      (each elt (tuple/slice ast 1 -2)
            (code* elt buf))
      (code* (last ast) buf))
    #
    :auto_resolve
    (buffer/push-string buf "::")
    #
    :symbolic
    (do
      (buffer/push-string buf "##")
      (buffer/push-string buf (in ast 1)))
    #
    :conditional
    (do
      (buffer/push-string buf "#?")
      (each elt (drop 1 ast)
            (code* elt buf)))
    #
    :conditional_splicing
    (do
      (buffer/push-string buf "#?@")
      (each elt (drop 1 ast)
            (code* elt buf)))
    #
    :eval
    (do
      (buffer/push-string buf "#=")
      (each elt (drop 1 ast)
            (code* elt buf)))
    ))

(defn code
  [ast]
  (let [buf @""]
    (code* ast buf)
    (string buf)))

(comment

  (code [:code [:keyword ":a"]])
  # => ":a"

  (code [:code [:number "1"]])
  # => "1"

  (code [:code [:whitespace " "]])
  # => " "

  (code [:code
         [:list
          [:number "1"] [:whitespace " "]
          [:number "2"]]])
  # => "(1 2)"

  (code [:code
         [:map
          [:keyword ":a"] [:whitespace " "]
          [:number "1"]]])
  # => "{:a 1}"

  (code [:code
         [:vector
          [:number "1"] [:whitespace " "]
          [:number "2"]]])
  # => "[1 2]"

  (code [:code
         [:set
          [:number "1"] [:whitespace " "]
          [:number "2"]]])
  # => "#{1 2}"

  (code [:code [:character "\\newline"]])
  # => "\\newline"

  (code [:code [:comment ";; hi"]])
  # => ";; hi"

  (code [:code [:string "\"smile\""]])
  # => "\"smile\""

  (code [:code [:symbol "a"]])
  # => "a"

  (code [:code [:regex "\".\""]])
  # => "#\".\""

  (code [:code [:quote [:symbol "a"]]])
  # => "'a"

  (code [:code
         [:quote
          [:list
           [:keyword ":a"]]]])
  # => "'(:a)"

  (code [:code
         [:fn
          [:list
           [:symbol "inc"] [:whitespace " "]
           [:symbol "%"]]]])
  # => "#(inc %)"

  (code [:code [:deref [:symbol "a"]]])
  # => "@a"

  (code [:code
         [:deref
          [:list
           [:symbol "atom"] [:whitespace " "]
           [:symbol "nil"]]]])
  # => "@(atom nil)"

  (code [:code [:backtick [:symbol "a"]]])
  # => "`a"

  (code [:code [:unquote [:symbol "a"]]])
  # => "~a"

  (code [:code [:unquote_splicing [:symbol "a"]]])
  # => "~@a"

  (code [:code
         [:discard
          [:whitespace " "] [:symbol "a"]]])
  # => "#_ a"

  (code [:code [:var_quote [:symbol "a"]]])
  # => "#'a"

  (code [:code
         [:tag
          [:symbol "uuid"] [:whitespace " "]
          [:string "\"00000000-0000-0000-0000-000000000000\""]]])
  # => "#uuid \"00000000-0000-0000-0000-000000000000\""

  (code [:code [:metadata
                [:metadata_entry
                 [:map
                  [:keyword ":a"] [:whitespace " "]
                  [:symbol "true"]]]
                [:whitespace " "]
                [:vector
                 [:keyword ":a"]]]])
  # => "^{:a true} [:a]"

  (code [:code [:metadata
                [:deprecated_metadata_entry
                 [:map
                  [:keyword ":a"] [:whitespace " "]
                  [:symbol "true"]]]
                [:whitespace " "]
                [:vector
                 [:keyword ":a"]]]])
  # => "#^{:a true} [:a]"

  (code [:code
         [:namespaced_map
          [:macro_keyword "::a"]
          [:map]]])
  # => "#::a{}"

  (code [:code
         [:namespaced_map
          [:auto_resolve]
          [:map]]])
  # => "#::{}"

  (code [:code
         [:namespaced_map
          [:keyword ":a"]
          [:map]]])
  # => "#:a{}"

  (code [:code [:macro_keyword "::a"]])
  # => "::a"

  (code [:code [:symbolic "Inf"]])
  # => "##Inf"

  (code [:code
         [:conditional
          [:list
           [:keyword ":clj"] [:whitespace " "]
           [:number "0"] [:whitespace " "]
           [:keyword ":cljr"] [:whitespace " "]
           [:number "1"]]]])
  # => "#?(:clj 0 :cljr 1)"

  (code [:code
          [:conditional_splicing
           [:list
            [:keyword ":clj"] [:whitespace " "]
            [:vector
             [:number "0"] [:whitespace " "]
             [:number "1"]] [:whitespace " "]
            [:keyword ":cljr"] [:whitespace " "]
            [:vector
             [:number "8"] [:whitespace " "]
             [:number "9"]]]]])
  # => "#?@(:clj [0 1] :cljr [8 9])"

  (code [:code [:eval [:symbol "a"]]])
  # => "#=a"

  (code [:code
         [:eval
          [:list
           [:symbol "+"] [:whitespace " "]
           [:symbol "a"] [:whitespace " "]
           [:symbol "b"]]]])
  # => "#=(+ a b)"

  )

(comment

  (defn round-trip
    [src]
    # houston, we have a property :)
    (code (ast src)))

  (round-trip ":a")
  # => ":a"

  (round-trip "1")
  # => "1"

  (round-trip " ")
  # => " "

  (round-trip "(1 2)")
  # => "(1 2)"

  (round-trip "{:a 1}")
  # => "{:a 1}"

  (round-trip "[1 2]")
  # => "[1 2]"

  (round-trip "#{1 2}")
  # => "#{1 2}"

  (round-trip "\\newline")
  # => "\\newline"

  (round-trip ";; hi")
  # => ";; hi"

  (round-trip "\"smile\"")
  # => "\"smile\""

  (round-trip "a")
  # => "a"

  (round-trip "#\".\"")
  # => "#\".\""

  (round-trip "'a")
  # => "'a"

  (round-trip "'(:a)")
  # => "'(:a)"

  (round-trip "#(inc %)")
  # => "#(inc %)"

  (round-trip "@a")
  # => "@a"

  (round-trip "@(atom nil)")
  # => "@(atom nil)"

  (round-trip "`a")
  # => "`a"

  (round-trip "~a")
  # => "~a"

  (round-trip "~@a")
  # => "~@a"

  (round-trip "#_ a")
  # => "#_ a"

  (round-trip "#'a")
  # => "#'a"

  (round-trip "#uuid \"00000000-0000-0000-0000-000000000000\"")
  # => "#uuid \"00000000-0000-0000-0000-000000000000\""

  (round-trip "^{:a true} [:a]")
  # => "^{:a true} [:a]"

  (round-trip "#^{:a true} [:a]")
  # => "#^{:a true} [:a]"

  (round-trip "#::a{}")
  # => "#::a{}"

  (round-trip "#::{}")
  # => "#::{}"

  (round-trip "#:a{}")
  # => "#:a{}"

  (round-trip "::a")
  # => "::a"

  (round-trip "##Inf")
  # => "##Inf"

  (round-trip "#?(:clj 0 :cljr 1)")
  # => "#?(:clj 0 :cljr 1)"

  (round-trip "#?@(:clj [0 1] :cljr [8 9])")
  # => "#?@(:clj [0 1] :cljr [8 9])"

  (round-trip "#=a")
  # => "#=a"

  (round-trip "#=(+ a b)")
  # => "#=(+ a b)"

  (comment

    (let [src (slurp (string (os/getenv "HOME")
                       "/src/clojure/src/clj/clojure/core.clj"))]
      (= (string src)
        (code (ast src))))

    )

  )
