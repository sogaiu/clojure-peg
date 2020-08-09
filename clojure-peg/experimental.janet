(import ./grammar :prefix "")

# make a version of cg that matches a single form
(def cg-one
  (->
   # cg is a struct, need something mutable
   (table ;(kvs cg))
   # just recognize one form
   (put :main :input)
   # tried using a table with a peg but had a problem, so use a struct
   table/to-struct))

(comment

 (peg/match cg-one "\"\\u0030\"")
 # => @[]

 (peg/match cg-one "(def a 1)")
 # => @[]

 (peg/match cg-one "[:a :b)")
 # ! "match error in range (6:6)"

 (peg/match cg-one "(def a ; hi\n 1)")
 # => @[]

 (peg/match cg-one "(def a ; hi 1)")
 # ! "match error in range (14:14)"

 (peg/match cg-one "[1]")
 # => @[]

 (peg/match cg-one "; hello")
 # => @[]

 (peg/match cg-one "8")
 # => @[]

 (peg/match cg-one "[:a :b]")
 # => @[]

 (peg/match cg-one "[:a :b] 1")
 # => @[]

 )

(def cg-capture-all
  (->
   # cg is a struct, need something mutable
   (table ;(kvs cg))
   # capture all
   (put :main '(capture (any :input)))
   # tried using a table with a peg but had a problem, so use a struct
   table/to-struct))

(comment

 (peg/match cg-capture-all "")
 # => @[""]

 (peg/match cg-capture-all "()")
 # => @["()"]

 (peg/match cg-capture-all "[]")
 # => @["[]"]

 (peg/match cg-capture-all "{}")
 # => @["{}"]

 (peg/match cg-capture-all "#{}")
 # => @["#{}"]

 (peg/match cg-capture-all "#::{}")
 # => @["#::{}"]

 (peg/match cg-capture-all "1")
 # => @["1"]

 (peg/match cg-capture-all "1/2")
 # => @["1/2"]

 (peg/match cg-capture-all ":a")
 # => @[":a"]

 (peg/match cg-capture-all "::a")
 # => @["::a"]

 (peg/match cg-capture-all "\"a\"")
 # => @["\"a\""]

 (peg/match cg-capture-all "#\".\"")
 # => @["#\".\""]

 (peg/match cg-capture-all "#_ a")
 # => @["#_ a"]

 (peg/match cg-capture-all "(def a 1) :a")
 # => @["(def a 1) :a"]

 )

(def cg-capture-one
  (->
   # cg is a struct, need something mutable
   (table ;(kvs cg))
   # capture one
   (put :main '(capture :input))
   # tried using a table with a peg but had a problem, so use a struct
   table/to-struct))

(comment

 (def sample-source
   (string ";; \"my test\"\n"
           "(+ 1 1)\n"
           ";; => 2\n"))

 (peg/match cg-capture-one sample-source)
 # => @[";; \"my test\""]

 (peg/match cg-capture-one sample-source 12)
 # => @["\n"]

 (peg/match cg-capture-one sample-source 13)
 # => @["(+ 1 1)"]

 (peg/match cg-capture-one sample-source 21)
 # => @[";; => 2"]

 )

(def cg-capture-top-levels
  (->
   # cg is a struct, need something mutable
   (table ;(kvs cg))
   # capture each top-level
   (put :main '(any (capture :input)))
   # tried using a table with a peg but had a problem, so use a struct
   table/to-struct))

(comment

 (peg/match cg-capture-top-levels "")
  # => @[]

 (peg/match cg-capture-top-levels "()")
  # => @["()"]

 (peg/match cg-capture-top-levels "[]")
 # => @["[]"]

 (peg/match cg-capture-top-levels "{}")
 # => @["{}"]

 (peg/match cg-capture-top-levels "#{}")
 # => @["#{}"]

 (peg/match cg-capture-top-levels "#::{}")
 # => @["#::{}"]

 (peg/match cg-capture-top-levels "1")
 # => @["1"]

 (peg/match cg-capture-top-levels "1/2")
 # => @["1/2"]

 (peg/match cg-capture-top-levels ":a")
 # => @[":a"]

 (peg/match cg-capture-top-levels "::a")
 # => @["::a"]

 (peg/match cg-capture-top-levels "\"a\"")
 # => @["\"a\""]

 (peg/match cg-capture-top-levels "#\".\"")
 # => @["#\".\""]

 (peg/match cg-capture-top-levels "#_ a")
 # => @["#_ a"]

 (peg/match cg-capture-top-levels "(def a 1) :a")
 # => @["(def a 1)" " " ":a"]

 (peg/match cg-capture-top-levels "^{:a true} [:a :b]")
 # => @["^{:a true} [:a :b]"]

 (peg/match cg-capture-top-levels "\\a")
 # => @["\\a"]

 )

(def cg-capture-ast-with-loc
  (->
    # cg is a struct, need something mutable
    (table ;(kvs cg))
    # override things that need to be captured
    (put :list
      ~(cmt
         (sequence (position)
                   (sequence "("
                             (capture (any :input))
                             (choice ")" (error "")))
                   (position))
         ,|[:list {:start (first $&)
                   :end (last $&)}
                  ;(slice $& 1 -3)]))
    (put :vector
      ~(cmt
         (sequence (position)
                   (sequence "["
                             (capture (any :input))
                             (choice "]" (error "")))
                   (position))
         ,|[:vector {:start (first $&)
                     :end (last $&)}
                    ;(slice $& 1 -3)]))
    (put :map
      ~(cmt
         (sequence (position)
                   (sequence "{"
                             (capture (any :input))
                             (choice "}" (error "")))
                   (position))
         ,|[:map {:start (first $&)
                  :end (last $&)}
                 ;(slice $& 1 -3)]))
    (put :keyword
      ~(cmt
         (sequence
           (position)
           (capture (sequence ":"
                              (choice "/"
                                      (sequence :keyword_head
                                                (any :keyword_body)))))
           (position))
         ,|[:keyword {:start (first $&)
                      :end (last $&)}
                     (in $& 1)]))
    (put :macro_keyword
         ~(cmt
            (sequence (position)
                      (capture (sequence "::"
                                         :keyword_head
                                         (any :keyword_body)))
                      (position))
            ,|[:macro_keyword {:start (first $&)
                               :end (last $&)}
                              (in $& 1)]))
    (put :string
         ~(cmt
            (sequence
              (position)
              (capture (sequence "\""
                                 (any (if-not (set "\"\\")
                                              1))
                                 (any (sequence "\\"
                                                1
                                                (any (if-not (set "\"\\")
                                                             1))))
                                 "\""))
                      (position))
            ,|[:string {:start (first $&)
                        :end (last $&)}
                       (in $& 1)]))
    (put :number
         ~(cmt
            (sequence (position)
                      (capture (sequence (opt (set "+-"))
                                         (some :digit)
                                         (choice :ratio_suffix
                                                 :double_suffix
                                                 :long_suffix)))
                      (position))
            ,|[:number {:start (first $&)
                        :end (last $&)}
                       (in $& 1)]))
    (put :character
         ~(cmt
            (sequence (position)
                      (capture (sequence "\\"
                                         (choice :named_char
                                                 :unicode
                                                 :unicode_char)))
                      (position))
            ,|[:character {:start (first $&)
                           :end (last $&)}
                          (in $& 1)]))
    (put :symbol
         ~(cmt
            (sequence (position)
                      (capture (sequence :name_head
                                         (any :name_body)))
                      (position))
            ,|[:symbol {:start (first $&)
                        :end (last $&)}
                       (in $& 1)]))
    (put :metadata
         ~(cmt
            (sequence
              (position)
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
              (position))
            ,|[:metadata {:start (first $&)
                          :end (last $&)}
                         ;(slice $& 1 -3)]))
    (put :metadata_entry
         ~(cmt
            (sequence (position)
                      (capture (sequence "^"
                                         (choice :map
                                                 :string
                                                 :macro_keyword
                                                 :keyword
                                                 :symbol)))
                      (position))
            ,|[:metadata_entry {:start (first $&)
                                :end (last $&)}
                               ;(slice $& 1 -3)]))
    (put :deprecated_metadata_entry
         ~(cmt
            (sequence
              (position)
              (capture (sequence "#^"
                                 (choice :map
                                         :string
                                         :macro_keyword
                                         :keyword
                                         :symbol)))
              (position))
            ,|[:deprecated_metadata_entry {:start (first $&)
                                           :end (last $&)}
                                          ;(slice $& 1 -3)]))
    (put :backtick
         ~(cmt
            (sequence (position)
                      (capture (sequence "`"
                                         (opt :whitespace)
                                         :form))
                      (position))
            ,|[:backtick {:start (first $&)
                          :end (last $&)}
                         ;(slice $& 1 -3)]))
    (put :quote
         ~(cmt
            (sequence (position)
                      (capture (sequence "'"
                                         (opt :whitespace)
                                         :form))
                      (position))
            ,|[:quote {:start (first $&)
                       :end (last $&)}
                      ;(slice $& 1 -3)]))
    (put :unquote
         ~(cmt
            (sequence (position)
                      (capture (sequence "~"
                                         (opt :whitespace)
                                         :form))
                      (position))
            ,|[:unquote {:start (first $&)
                         :end (last $&)}
                        ;(slice $& 1 -3)]))
    (put :unquote_splicing
         ~(cmt
            (sequence (position)
                      (capture (sequence "~@"
                                         (opt :whitespace)
                                         :form))
                      (position))
            ,|[:unquote_splicing {:start (first $&)
                                  :end (last $&)}
                                 ;(slice $& 1 -3)]))
    (put :deref
         ~(cmt
            (sequence (position)
                      (capture (sequence "@"
                                         (opt :whitespace)
                                         :form))
                      (position))
            ,|[:deref {:start (first $&)
                       :end (last $&)}
                      ;(slice $& 1 -3)]))
    (put :fn
         ~(cmt
            (sequence (position)
                      (sequence "#"
                                (capture :list))
                      (position))
            ,|[:fn {:start (first $&)
                    :end (last $&)}
                   ;(slice $& 1 -3)]))
    (put :regex
         ~(cmt
            (sequence (position)
                      (sequence "#"
                                (capture :string))
                      (position))
            ,|[:regex {:start (first $&)
                       :end (last $&)}
                      (last (in $& 1))]))
    (put :set
         ~(cmt
            (sequence (position)
                      (sequence "#{"
                                (capture (any :input))
                                (choice "}" (error "")))
                      (position))
            ,|[:set {:start (first $&)
                     :end (last $&)}
                    ;(slice $& 1 -3)]))
    (put :namespaced_map
         ~(cmt
            (sequence (position)
                      (capture (sequence "#"
                                         (choice :macro_keyword
                                                 :auto_resolve
                                                 :keyword)
                                         (opt :whitespace)
                                         :map))
                      (position))
            ,|[:namespaced_map {:start (first $&)
                                :end (last $&)}
                               ;(slice $& 1 -3)]))
    (put :auto_resolve
         ~(cmt
            (sequence (position)
                      (capture "::")
                      (position))
            ,(fn [& caps]
               [:auto_resolve {:start (first caps)
                               :end (last caps)}])))
    (put :var_quote
         ~(cmt
            (sequence (position)
                      (capture (sequence "#'"
                                         (opt :whitespace)
                                         :form))
                      (position))
            ,|[:var_quote {:start (first $&)
                           :end (last $&)}
                          ;(slice $& 1 -3)]))
    (put :discard
         ~(cmt
            (sequence (position)
                      (capture (sequence "#_"
                                         (opt (sequence (opt :whitespace)
                                                        :discard))
                                         (opt :whitespace)
                                         :form))
                      (position))
            ,|[:discard  {:start (first $&)
                          :end (last $&)}
                         ;(slice $& 1 -3)]))
    (put :tag
         ~(cmt
            (sequence (position)
                      (capture (sequence "#"
                                         :symbol
                                         (opt :whitespace)
                                         (choice :tag
                                                 :collection
                                                 :literal)))
                      (position))
            ,|[:tag {:start (first $&)
                     :end (last $&)}
                    ;(slice $& 1 -3)]))
    (put :conditional
         ~(cmt
            (sequence (position)
                      (capture (sequence "#?"
                                         (opt :whitespace)
                                         :list))
                      (position))
            ,|[:conditional {:start (first $&)
                             :end (last $&)}
                            ;(slice $& 1 -3)]))
    (put :conditional_splicing
         ~(cmt
            (sequence (position)
                      (capture (sequence "#?@"
                                         (opt :whitespace)
                                         :list))
                      (position))
            ,|[:conditional_splicing {:start (first $&)
                                      :end (last $&)}
                                     ;(slice $& 1 -3)]))
    (put :symbolic
         ~(cmt
            (sequence (position)
                      (capture (sequence "##"
                                         :symbol))
                      (position))
            ,|[:symbolic {:start (first $&)
                          :end (last $&)}
                         (last ;(slice $& 1 -3))]))
    (put :eval
         ~(cmt
            (sequence (position)
                      (capture (sequence "#="
                                         (opt :whitespace)
                                         (choice :list
                                                 :symbol)))
                      (position))
            ,|[:eval {:start (first $&)
                      :end (last $&)}
                     ;(slice $& 1 -3)]))
    (put :whitespace
         ~(cmt
            (sequence (position)
                      (capture (some (set "\f\n\r\t, ")))
                      (position))
            ,|[:whitespace {:start (first $&)
                            :end (last $&)}
                           (in $& 1)]))
    (put :comment
         ~(cmt (sequence (position)
                         (capture (sequence (choice ";"
                                                    "#!")
                                            (any (if-not (set "\r\n")
                                                   1))))
                         (position))
               ,|[:comment {:start (first $&)
                            :end (last $&)}
                           (in $& 1)]))
    # tried using a table with a peg but had a problem, so use a struct
    table/to-struct))

(comment

  (peg/match cg-capture-ast-with-loc ":a")
  # => @[[:keyword {:start 0 :end 2} ":a"]]

  (peg/match cg-capture-ast-with-loc "\"smile\"")
  # => @[[:string {:start 0 :end 7} "\"smile\""]]

  (peg/match cg-capture-ast-with-loc "1/2")
  # => @[[:number {:start 0 :end 3} "1/2"]]

  (peg/match cg-capture-ast-with-loc "defmacro")
  # => @[[:symbol {:start 0 :end 8} "defmacro"]]

  (peg/match cg-capture-ast-with-loc "::a")
  # => @[[:macro_keyword {:start 0 :end 3} "::a"]]

  (peg/match cg-capture-ast-with-loc "\\a")
  # => @[[:character {:start 0 :end 2} "\\a"]]

  (peg/match cg-capture-ast-with-loc "{}")
  # => @[[:map {:start 0 :end 2}]]

  (peg/match cg-capture-ast-with-loc "{:a 1}")
  ``
  @[[:map {:start 0 :end 6}
     [:keyword {:start 1 :end 3} ":a"]
     [:whitespace {:start 3 :end 4} " "]
     [:number {:start 4 :end 5} "1"]]]
  ``

  (peg/match cg-capture-ast-with-loc "#::{}")
  ``
  @[[:namespaced_map {:start 0 :end 5}
     [:auto_resolve {:start 1 :end 3}]
     [:map {:start 3 :end 5}]]]
  ``

  (peg/match cg-capture-ast-with-loc "#::a{}")
  ``
  @[[:namespaced_map {:start 0 :end 6}
     [:macro_keyword {:start 1 :end 4} "::a"]
     [:map {:start 4 :end 6}]]]
  ``

  (peg/match cg-capture-ast-with-loc "#:a{}")
  ``
  @[[:namespaced_map {:start 0 :end 5}
     [:keyword {:start 1 :end 3} ":a"]
     [:map {:start 3 :end 5}]]]
  ``

  (peg/match cg-capture-ast-with-loc "[]")
  # => @[[:vector {:start 0 :end 2}]]

  (peg/match cg-capture-ast-with-loc "[:a]")
  ``
  @[[:vector {:start 0 :end 4}
     [:keyword {:start 1 :end 3} ":a"]]]
  ``

  (peg/match cg-capture-ast-with-loc "()")
  # => @[[:list {:start 0 :end 2}]]

  (peg/match cg-capture-ast-with-loc "(:a)")
  ``
  @[[:list {:start 0 :end 4}
     [:keyword {:start 1 :end 3} ":a"]]]
  ``

  (peg/match cg-capture-ast-with-loc "^{:a true} [:a]")
  ``
  @[[:metadata {:start 0 :end 15}
     [:metadata_entry {:start 0 :end 10}
      [:map {:start 1 :end 10}
       [:keyword {:start 2 :end 4} ":a"]
       [:whitespace {:start 4 :end 5} " "]
       [:symbol {:start 5 :end 9} "true"]]]
     [:whitespace {:start 10 :end 11} " "]
     [:vector {:start 11 :end 15}
      [:keyword {:start 12 :end 14} ":a"]]]]
  ``

  (peg/match cg-capture-ast-with-loc "#^{:a true} [:a]")
  ``
  @[[:metadata {:start 0 :end 16}
     [:deprecated_metadata_entry {:start 0 :end 11}
      [:map {:start 2 :end 11}
       [:keyword {:start 3 :end 5} ":a"]
       [:whitespace {:start 5 :end 6} " "]
       [:symbol {:start 6 :end 10} "true"]]]
     [:whitespace {:start 11 :end 12} " "]
     [:vector {:start 12 :end 16}
      [:keyword {:start 13 :end 15} ":a"]]]]
  ``

  (peg/match cg-capture-ast-with-loc "`a")
  ``
  @[[:backtick {:start 0 :end 2}
     [:symbol {:start 1 :end 2} "a"]]]
  ``

  (peg/match cg-capture-ast-with-loc "'a")
  ``
  @[[:quote {:start 0 :end 2}
     [:symbol {:start 1 :end 2} "a"]]]
  ``

  (peg/match cg-capture-ast-with-loc "~a")
  ``
  @[[:unquote {:start 0 :end 2}
     [:symbol {:start 1 :end 2} "a"]]]
  ``

  (peg/match cg-capture-ast-with-loc "~@a")
  ``
  @[[:unquote_splicing {:start 0 :end 3}
     [:symbol {:start 2 :end 3} "a"]]]
  ``

  (peg/match cg-capture-ast-with-loc "@a")
  ``
  @[[:deref {:start 0 :end 2}
     [:symbol {:start 1 :end 2} "a"]]]
  ``

  (peg/match cg-capture-ast-with-loc "#(inc %)")
  ``
  @[[:fn {:start 0 :end 8}
     [:list {:start 1 :end 8}
      [:symbol {:start 2 :end 5} "inc"]
      [:whitespace {:start 5 :end 6} " "]
      [:symbol {:start 6 :end 7} "%"]]]]
  ``

  (peg/match cg-capture-ast-with-loc "#\".\"")
  # => @[[:regex {:start 0 :end 4} "\".\""]]

  (peg/match cg-capture-ast-with-loc "#{:a}")
  ``
  @[[:set {:start 0 :end 5}
     [:keyword {:start 2 :end 4} ":a"]]]
  ``

  (peg/match cg-capture-ast-with-loc "#'a")
  ``
  @[[:var_quote {:start 0 :end 3}
     [:symbol {:start 2 :end 3} "a"]]]
  ``

  (peg/match cg-capture-ast-with-loc "#_ a")
  ``
  @[[:discard {:start 0 :end 4}
     [:whitespace {:start 2 :end 3} " "]
     [:symbol {:start 3 :end 4} "a"]]]
  ``

  (peg/match cg-capture-ast-with-loc
    "#uuid \"00000000-0000-0000-0000-000000000000\"")
  ``
  @[[:tag {:start 0 :end 44}
     [:symbol {:start 1 :end 5} "uuid"]
     [:whitespace {:start 5 :end 6} " "]
     [:string {:start 6 :end 44} "\"00000000-0000-0000-0000-000000000000\""]]]
  ``

  (peg/match cg-capture-ast-with-loc " ")
  # => @[[:whitespace {:start 0 :end 1} " "]]

  (peg/match cg-capture-ast-with-loc "; hey")
  # => @[[:comment {:start 0 :end 5} "; hey"]]

  (peg/match cg-capture-ast-with-loc "#! foo")
  # => @[[:comment {:start 0 :end 6} "#! foo"]]

  (peg/match cg-capture-ast-with-loc "#?(:clj 0 :cljr 1)")
  ``
  @[[:conditional {:start 0 :end 18}
     [:list {:start 2 :end 18}
      [:keyword {:start 3 :end 7} ":clj"]
      [:whitespace {:start 7 :end 8} " "]
      [:number {:start 8 :end 9} "0"]
      [:whitespace {:start 9 :end 10} " "]
      [:keyword {:start 10 :end 15} ":cljr"]
      [:whitespace {:start 15 :end 16} " "]
      [:number {:start 16 :end 17} "1"]]]]
  ``

  (peg/match cg-capture-ast-with-loc "#?@(:clj [0 1] :cljr [1 2])")
  ``
  @[[:conditional_splicing {:start 0 :end 27}
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
  ``

  (peg/match cg-capture-ast-with-loc "##NaN")
  # => @[[:symbolic {:start 0 :end 5} "NaN"]]

  (peg/match cg-capture-ast-with-loc "#=a")
  ``
  @[[:eval {:start 0 :end 3}
     [:symbol {:start 2 :end 3} "a"]]]
  ``

  )

# XXX: apply the same override technique used above below here

(def cg-capture-ast-dont-store-loc
  ~{:main (any :input)
    #
    :input (choice :whitespace
                   :comment
                   :discard
                   :form)
    #
    :form (choice :reader_macro
                  :collection
                  :literal)
    #
    :collection (choice :list
                        :vector
                        :map)
    #
    :list
    (cmt
      (sequence (position)
                (sequence "("
                          (capture (any :input))
                          (choice ")" (error "")))
                (position))
      ,|[:list ;(slice $& 1 -3)])
    #
    :vector
    (cmt
      (sequence (position)
                (sequence "["
                          (capture (any :input))
                          (choice "]" (error "")))
                (position))
      ,|[:vector ;(slice $& 1 -3)])
    #
    :map
    (cmt
      (sequence (position)
                (sequence "{"
                          (capture (any :input))
                          (choice "}" (error "")))
                (position))
      ,|[:map ;(slice $& 1 -3)])
    #
    :literal (choice :number
                     :macro_keyword
                     :keyword
                     :string
                     :character
                     :symbol)
    #
    :keyword
    (cmt
      (sequence (position)
                (capture (sequence ":"
                                   (choice "/"
                                           (sequence :keyword_head
                                                     (any :keyword_body)))))
                (position))
      ,|[:keyword (in $& 1)])
    #
    :keyword_head (choice :allowed_name_character
                          (set "#'"))
    #
    :allowed_name_character (if-not (set "\r\n\t\f ()[]{}\"@~^;`\\,:#'/")
                                    1)
    #
    :keyword_body (choice (set ":/")
                          :keyword_head)
    #
    :macro_keyword
    (cmt
      (sequence (position)
                (capture (sequence "::"
                                   :keyword_head
                                   (any :keyword_body)))
                (position))
      ,|[:macro_keyword (in $& 1)])
    #
    :string
    (cmt
      (sequence (position)
                (capture (sequence "\""
                                   (any (if-not (set "\"\\")
                                                1))
                                   (any (sequence "\\"
                                                  1
                                                  (any (if-not (set "\"\\")
                                                               1))))
                                   "\""))
                (position))
      ,|[:string (in $& 1)])
    #
    :number
    (cmt
      (sequence (position)
                (capture (sequence (opt (set "+-"))
                                   (some :digit)
                                   (choice :ratio_suffix
                                           :double_suffix
                                           :long_suffix)))
                (position))
      ,|[:number (in $& 1)])
    #
    :digit (range "09")
    #
    :double_suffix
    (sequence (sequence (opt (sequence "."
                                       (any :digit)))
                        (opt (sequence (set "eE")
                                       (opt (set "+-"))
                                       (some :digit))))
              (opt "M"))
    #
    :long_suffix
    (sequence (opt (choice (sequence (set "xX")
                                     (some (range "09" "af" "AF")))
                           (sequence (set "rR")
                                     (some (range "09" "az" "AZ")))
                           (some (range "07"))))
              (opt "N"))
    #
    :ratio_suffix (sequence "/"
                            (some :digit))
    #
    :character
    (cmt
      (sequence (position)
                (capture (sequence "\\"
                                   (choice :named_char
                                           :unicode
                                           :unicode_char)))
                (position))
      ,|[:character (in $& 1)])
    # XXX: this just matches anything...may be not what we want
    :unicode_char 1
    #
    :named_char (choice "backspace"
                        "formfeed"
                        "newline"
                        "return"
                        "space"
                        "tab")
    #
    :unicode (sequence "u"
                       (range "09" "af" "AF")
                       (range "09" "af" "AF")
                       (range "09" "af" "AF")
                       (range "09" "af" "AF"))
    #
    :symbol
    (cmt
      (sequence (position)
                (capture (sequence :name_head
                                   (any :name_body)))
                (position))
      ,|[:symbol (in $& 1)])
    #
    :name_head (choice "/"
                       :allowed_name_character)
    #
    :name_body (choice :name_head
                       (set ":#'"))
    #
    :reader_macro (choice :dispatch
                          :backtick
                          :quote
                          :unquote_splicing
                          :unquote
                          :deref
                          :metadata)
    #
    :metadata
    (cmt
      (sequence
        (position)
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
        (position))
      ,|[:metadata ;(slice $& 1 -3)])
    #
    :metadata_entry
    (cmt
      (sequence (position)
                (capture (sequence "^"
                                   (choice :map
                                           :string
                                           :macro_keyword
                                           :keyword
                                           :symbol)))
                (position))
      ,|[:metadata_entry ;(slice $& 1 -3)])
    #
    :deprecated_metadata_entry
    (cmt
      (sequence
        (position)
        (capture (sequence "#^"
                            (choice :map
                                    :string
                                    :macro_keyword
                                    :keyword
                                    :symbol)))
        (position))
      ,|[:deprecated_metadata_entry ;(slice $& 1 -3)])
    #
    :backtick
    (cmt
      (sequence (position)
                (capture (sequence "`"
                                   (opt :whitespace)
                                   :form))
                (position))
      ,|[:backtick ;(slice $& 1 -3)])
    #
    :quote
    (cmt
      (sequence (position)
                (capture (sequence "'"
                                   (opt :whitespace)
                                   :form))
                (position))
      ,|[:quote ;(slice $& 1 -3)])
    #
    :unquote
    (cmt
      (sequence (position)
                (capture (sequence "~"
                                   (opt :whitespace)
                                   :form))
                (position))
      ,|[:unquote ;(slice $& 1 -3)])
    #
    :unquote_splicing
    (cmt
      (sequence (position)
                (capture (sequence "~@"
                                   (opt :whitespace)
                                   :form))
                (position))
      ,|[:unquote_splicing ;(slice $& 1 -3)])
    #
    :deref
    (cmt
      (sequence (position)
                (capture (sequence "@"
                                   (opt :whitespace)
                                   :form))
                (position))
      ,|[:deref ;(slice $& 1 -3)])
    #
    :dispatch (choice :set
                      :fn
                      :regex
                      :conditional
                      :conditional_splicing
                      :namespaced_map
                      :var_quote
                      :eval
                      :tag
                      :symbolic)
    #
    :fn (cmt
          (sequence (position)
                    (sequence "#"
                              (capture :list))
                    (position))
          ,|[:fn ;(slice $& 1 -3)])
    #
    :regex (cmt
             (sequence (position)
                       (sequence "#"
                                 (capture :string))
                       (position))
             ,|[:regex (last (in $& 1))])
    #
    :set (cmt
           (sequence (position)
                     (sequence "#{"
                               (capture (any :input))
                               (choice "}" (error "")))
                     (position))
           ,|[:set ;(slice $& 1 -3)])
    #
    :namespaced_map
    (cmt
      (sequence (position)
                (capture (sequence "#"
                                   (choice :macro_keyword
                                           :auto_resolve
                                           :keyword)
                                   (opt :whitespace)
                                   :map))
                (position))
      ,|[:namespaced_map ;(slice $& 1 -3)])
    #
    :auto_resolve
    (cmt
      (sequence (position)
                (capture "::")
                (position))
      ,(fn [& caps]
         [:auto_resolve]))
    #
    :var_quote (cmt (sequence (position)
                              (capture (sequence "#'"
                                                 (opt :whitespace)
                                                 :form))
                              (position))
                    ,|[:var_quote ;(slice $& 0 -2)])
    #
    :discard (cmt (sequence (position)
                            (capture (sequence "#_"
                                               (opt (sequence (opt :whitespace)
                                                              :discard))
                                               (opt :whitespace)
                                               :form))
                            (position))
                  ,|[:discard ;(slice $& 0 -2)])
    #
    :tag (cmt
           (sequence (position)
                     (capture (sequence "#"
                                        :symbol
                                        (opt :whitespace)
                                        (choice :tag
                                                :collection
                                                :literal)))
                     (position))
           ,|[:tag ;(slice $& 1 -3)])
    #
    :conditional (cmt
                   (sequence (position)
                             (capture (sequence "#?"
                                                (opt :whitespace)
                                                :list))
                             (position))
                   ,|[:conditional ;(slice $& 1 -3)])
    #
    :conditional_splicing
    (cmt
      (sequence (position)
                (capture (sequence "#?@"
                                   (opt :whitespace)
                                   :list))
                (position))
      ,|[:conditional_splicing ;(slice $& 1 -3)])
    #
    :symbolic (cmt
                (sequence (position)
                          (capture (sequence "##"
                                     :symbol))
                          (position))
                ,|[:symbolic (last ;(slice $& 1 -3))])
    #
    :eval (cmt
            (sequence (position)
                      (capture (sequence "#="
                                         (opt :whitespace)
                                         (choice :list
                                                 :symbol)))
                      (position))
            ,|[:eval ;(slice $& 1 -3)])
    #
    :whitespace (cmt
                  (sequence (position)
                            (capture (some (set "\f\n\r\t, ")))
                            (position))
                  ,|[:whitespace (in $& 1)])
    #
    :comment (cmt (sequence (position)
                            (capture (sequence (choice ";"
                                                       "#!")
                                               (any (if-not (set "\r\n")
                                                            1))))
                            (position))
                  ,|[:comment (in $& 1)])
    })

(def cg-capture-ast-with-start-match-and-store-loc
  ~{:main (any :input)
    #
    :input (choice :whitespace
                   :comment
                   :discard
                   :form)
    #
    :form (choice :reader_macro
                  :collection
                  :literal)
    #
    :collection (choice :list
                        :vector
                        :map)
    #
    :list
    (cmt
      (sequence (position)
                (sequence "("
                          (capture (any :input))
                          (choice ")" (error ""))))
      ,|[:list {:start (first $&)}
               ;(slice $& 1 -2)])
    #
    :vector
    (cmt
      (sequence (position)
                (sequence "["
                          (capture (any :input))
                          (choice "]" (error ""))))
      ,|[:vector {:start (first $&)}
                 ;(slice $& 1 -2)])
    #
    :map
    (cmt
      (sequence (position)
                (sequence "{"
                          (capture (any :input))
                          (choice "}" (error ""))))
      ,|[:map {:start (first $&)}
              ;(slice $& 1 -2)])
    #
    :literal (choice :number
                     :macro_keyword
                     :keyword
                     :string
                     :character
                     :symbol)
    #
    :keyword
    (cmt
      (sequence (position)
                (capture (sequence ":"
                                   (choice "/"
                                           (sequence :keyword_head
                                                     (any :keyword_body))))))
      ,|[:keyword {:start (first $&)}
                  (in $& 1)])
    #
    :keyword_head (choice :allowed_name_character
                          (set "#'"))
    #
    :allowed_name_character (if-not (set "\r\n\t\f ()[]{}\"@~^;`\\,:#'/")
                                    1)
    #
    :keyword_body (choice (set ":/")
                          :keyword_head)
    #
    :macro_keyword
    (cmt
      (sequence (position)
                (capture (sequence "::"
                                   :keyword_head
                                   (any :keyword_body))))
      ,|[:macro_keyword {:start (first $&)}
                        (in $& 1)])
    #
    :string
    (cmt
      (sequence (position)
                (capture (sequence "\""
                                   (any (if-not (set "\"\\")
                                                1))
                                   (any (sequence "\\"
                                                  1
                                                  (any (if-not (set "\"\\")
                                                               1))))
                                   "\"")))
      ,|[:string {:start (first $&)}
                 (in $& 1)])
    #
    :number
    (cmt
      (sequence (position)
                (capture (sequence (opt (set "+-"))
                                   (some :digit)
                                   (choice :ratio_suffix
                                           :double_suffix
                                           :long_suffix))))
      ,|[:number {:start (first $&)}
                  (in $& 1)])
    #
    :digit (range "09")
    #
    :double_suffix
    (sequence (sequence (opt (sequence "."
                                       (any :digit)))
                        (opt (sequence (set "eE")
                                       (opt (set "+-"))
                                       (some :digit))))
              (opt "M"))
    #
    :long_suffix
    (sequence (opt (choice (sequence (set "xX")
                                     (some (range "09" "af" "AF")))
                           (sequence (set "rR")
                                     (some (range "09" "az" "AZ")))
                           (some (range "07"))))
              (opt "N"))
    #
    :ratio_suffix (sequence "/"
                            (some :digit))
    #
    :character
    (cmt
      (sequence (position)
                (capture (sequence "\\"
                                   (choice :named_char
                                           :unicode
                                           :unicode_char))))
      ,|[:character {:start (first $&)}
                    (in $& 1)])
    # XXX: this just matches anything...may be not what we want
    :unicode_char 1
    #
    :named_char (choice "backspace"
                        "formfeed"
                        "newline"
                        "return"
                        "space"
                        "tab")
    #
    :unicode (sequence "u"
                       (range "09" "af" "AF")
                       (range "09" "af" "AF")
                       (range "09" "af" "AF")
                       (range "09" "af" "AF"))
    #
    :symbol
    (cmt
      (sequence (position)
                (capture (sequence :name_head
                                   (any :name_body))))
      ,|[:symbol {:start (first $&)}
                  (in $& 1)])
    #
    :name_head (choice "/"
                       :allowed_name_character)
    #
    :name_body (choice :name_head
                       (set ":#'"))
    #
    :reader_macro (choice :dispatch
                          :backtick
                          :quote
                          :unquote_splicing
                          :unquote
                          :deref
                          :metadata)
    #
    :metadata
    (cmt
      (sequence
        (position)
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
                            :symbol))))
      ,|[:metadata {:start (first $&)}
                   ;(slice $& 1 -2)])
    #
    :metadata_entry
    (cmt
      (sequence (position)
                (capture (sequence "^"
                                   (choice :map
                                           :string
                                           :macro_keyword
                                           :keyword
                                           :symbol))))
      ,|[:metadata_entry {:start (first $&)}
                         ;(slice $& 1 -2)])
    #
    :deprecated_metadata_entry
    (cmt
      (sequence
        (position)
        (capture (sequence "#^"
                            (choice :map
                                    :string
                                    :macro_keyword
                                    :keyword
                                    :symbol))))
      ,|[:deprecated_metadata_entry {:start (first $&)}
                                    ;(slice $& 1 -2)])
    #
    :backtick
    (cmt
      (sequence (position)
                (capture (sequence "`"
                                   (opt :whitespace)
                                   :form)))
      ,|[:backtick {:start (first $&)}
                   ;(slice $& 1 -2)])
    #
    :quote
    (cmt
      (sequence (position)
                (capture (sequence "'"
                                   (opt :whitespace)
                                   :form)))
      ,|[:quote {:start (first $&)}
                ;(slice $& 1 -2)])
    #
    :unquote
    (cmt
      (sequence (position)
                (capture (sequence "~"
                                   (opt :whitespace)
                                   :form)))
      ,|[:unquote {:start (first $&)}
                  ;(slice $& 1 -2)])
    #
    :unquote_splicing
    (cmt
      (sequence (position)
                (capture (sequence "~@"
                                   (opt :whitespace)
                                   :form)))
      ,|[:unquote_splicing {:start (first $&)}
                           ;(slice $& 1 -2)])
    #
    :deref
    (cmt
      (sequence (position)
                (capture (sequence "@"
                                   (opt :whitespace)
                                   :form)))
      ,|[:deref {:start (first $&)}
                ;(slice $& 1 -2)])
    #
    :dispatch (choice :set
                      :fn
                      :regex
                      :conditional
                      :conditional_splicing
                      :namespaced_map
                      :var_quote
                      :eval
                      :tag
                      :symbolic)
    #
    :fn (cmt
          (sequence (position)
                    (sequence "#"
                              (capture :list)))
          ,|[:fn {:start (first $&)}
                 ;(slice $& 1 -2)])
    #
    :regex (cmt
             (sequence (position)
                       (sequence "#"
                                 (capture :string)))
             ,|[:regex {:start (first $&)}
                       (last (in $& 1))])
    #
    :set (cmt
           (sequence (position)
                     (sequence "#{"
                               (capture (any :input))
                               (choice "}" (error ""))))
           ,|[:set {:start (first $&)}
                   ;(slice $& 1 -2)])
    #
    :namespaced_map
    (cmt
      (sequence (position)
                (capture (sequence "#"
                                   (choice :macro_keyword
                                           :auto_resolve
                                           :keyword)
                                   (opt :whitespace)
                                   :map)))
      ,|[:namespaced_map {:start (first $&)}
                         ;(slice $& 1 -2)])
    #
    :auto_resolve
    (cmt
      (sequence (position)
                (capture "::"))
      ,(fn [& caps]
         [:auto_resolve {:start (first caps)}]))
    #
    :var_quote (cmt
                 (sequence (position)
                           (capture (sequence "#'"
                                              (opt :whitespace)
                                              :form)))
                ,|[:var_quote {:start (first $&)}
                              ;(slice $& 1 -2)])
    #
    :discard (cmt
               (sequence (position)
                         (capture (sequence "#_"
                                            (opt (sequence (opt :whitespace)
                                                           :discard))
                                            (opt :whitespace)
                                            :form)))
               ,|[:discard  {:start (first $&)}
                            ;(slice $& 1 -2)])
    #
    :tag (cmt
           (sequence (position)
                     (capture (sequence "#"
                                        :symbol
                                        (opt :whitespace)
                                        (choice :tag
                                                :collection
                                                :literal))))
           ,|[:tag {:start (first $&)}
                    ;(slice $& 1 -2)])
    #
    :conditional (cmt
                   (sequence (position)
                             (capture (sequence "#?"
                                                (opt :whitespace)
                                                :list)))
                   ,|[:conditional {:start (first $&)}
                                   ;(slice $& 1 -2)])
    #
    :conditional_splicing
    (cmt
      (sequence (position)
                (capture (sequence "#?@"
                                   (opt :whitespace)
                                   :list)))
      ,|[:conditional_splicing {:start (first $&)}
                               ;(slice $& 1 -2)])
    #
    :symbolic (cmt
                (sequence (position)
                          (capture (sequence "##"
                                     :symbol)))
                ,|[:symbolic {:start (first $&)}
                             (last ;(slice $& 1 -2))])
    #
    :eval (cmt
            (sequence (position)
                      (capture (sequence "#="
                                         (opt :whitespace)
                                         (choice :list
                                                 :symbol))))
            ,|[:eval {:start (first $&)}
                     ;(slice $& 1 -2)])
    #
    :whitespace (cmt
                  (sequence (position)
                            (capture (some (set "\f\n\r\t, "))))
                  ,|[:whitespace {:start (first $&)}
                                 (in $& 1)])
    #
    :comment (cmt (sequence (position)
                            (capture (sequence (choice ";"
                                                       "#!")
                                               (any (if-not (set "\r\n")
                                                            1)))))
                  ,|[:comment {:start (first $&)}
                              (in $& 1)])
    })

(comment

  (peg/match cg-capture-ast-with-start-match-and-store-loc ":a")
  # => @[[:keyword {:start 0} ":a"]]

  (peg/match cg-capture-ast-with-start-match-and-store-loc "\"smile\"")
  # => @[[:string {:start 0} "\"smile\""]]

  (peg/match cg-capture-ast-with-start-match-and-store-loc "1/2")
  # => @[[:number {:start 0} "1/2"]]

  (peg/match cg-capture-ast-with-start-match-and-store-loc "defmacro")
  # => @[[:symbol {:start 0} "defmacro"]]

  (peg/match cg-capture-ast-with-start-match-and-store-loc "::a")
  # => @[[:macro_keyword {:start 0} "::a"]]

  (peg/match cg-capture-ast-with-start-match-and-store-loc "\\a")
  # => @[[:character {:start 0} "\\a"]]

  (peg/match cg-capture-ast-with-start-match-and-store-loc "{}")
  # => @[[:map {:start 0}]]

  (peg/match cg-capture-ast-with-start-match-and-store-loc "{:a 1}")
  ``
  @[[:map {:start 0}
     [:keyword {:start 1} ":a"]
     [:whitespace {:start 3} " "]
     [:number {:start 4} "1"]]]
  ``

  (peg/match cg-capture-ast-with-start-match-and-store-loc "#::{}")
  ``
  @[[:namespaced_map {:start 0}
     [:auto_resolve {:start 1}]
     [:map {:start 3}]]]
  ``

  (peg/match cg-capture-ast-with-start-match-and-store-loc "#::a{}")
  ``
  @[[:namespaced_map {:start 0}
     [:macro_keyword {:start 1} "::a"]
     [:map {:start 4}]]]
  ``

  (peg/match cg-capture-ast-with-start-match-and-store-loc "#:a{}")
  ``
  @[[:namespaced_map {:start 0}
     [:keyword {:start 1} ":a"]
     [:map {:start 3}]]]
  ``

  (peg/match cg-capture-ast-with-start-match-and-store-loc "[]")
  # => @[[:vector {:start 0}]]

  (peg/match cg-capture-ast-with-start-match-and-store-loc "[:a]")
  ``
  @[[:vector {:start 0}
     [:keyword {:start 1} ":a"]]]
  ``

  (peg/match cg-capture-ast-with-start-match-and-store-loc "()")
  # => @[[:list {:start 0}]]

  (peg/match cg-capture-ast-with-start-match-and-store-loc "(:a)")
  ``
  @[[:list {:start 0}
     [:keyword {:start 1} ":a"]]]
  ``

  (peg/match cg-capture-ast-with-start-match-and-store-loc "^{:a true} [:a]")
  ``
  @[[:metadata {:start 0}
     [:metadata_entry {:start 0}
      [:map {:start 1}
       [:keyword {:start 2} ":a"]
       [:whitespace {:start 4} " "]
       [:symbol {:start 5} "true"]]]
     [:whitespace {:start 10} " "]
     [:vector {:start 11}
      [:keyword {:start 12} ":a"]]]]
  ``

  (peg/match cg-capture-ast-with-start-match-and-store-loc "#^{:a true} [:a]")
  ``
  @[[:metadata {:start 0}
     [:deprecated_metadata_entry {:start 0}
      [:map {:start 2}
       [:keyword {:start 3} ":a"]
       [:whitespace {:start 5} " "]
       [:symbol {:start 6} "true"]]]
     [:whitespace {:start 11} " "]
     [:vector {:start 12}
      [:keyword {:start 13} ":a"]]]]
  ``

  (peg/match cg-capture-ast-with-start-match-and-store-loc "`a")
  ``
  @[[:backtick {:start 0}
     [:symbol {:start 1} "a"]]]
  ``

  (peg/match cg-capture-ast-with-start-match-and-store-loc "'a")
  ``
  @[[:quote {:start 0}
     [:symbol {:start 1} "a"]]]
  ``

  (peg/match cg-capture-ast-with-start-match-and-store-loc "~a")
  ``
  @[[:unquote {:start 0}
     [:symbol {:start 1} "a"]]]
  ``

  (peg/match cg-capture-ast-with-start-match-and-store-loc "~@a")
  ``
  @[[:unquote_splicing {:start 0}
     [:symbol {:start 2} "a"]]]
  ``

  (peg/match cg-capture-ast-with-start-match-and-store-loc "@a")
  ``
  @[[:deref {:start 0}
     [:symbol {:start 1} "a"]]]
  ``

  (peg/match cg-capture-ast-with-start-match-and-store-loc "#(inc %)")
  ``
  @[[:fn {:start 0}
     [:list {:start 1}
      [:symbol {:start 2} "inc"]
      [:whitespace {:start 5} " "]
      [:symbol {:start 6} "%"]]]]
  ``

  (peg/match cg-capture-ast-with-start-match-and-store-loc "#\".\"")
  # => @[[:regex {:start 0} "\".\""]]

  (peg/match cg-capture-ast-with-start-match-and-store-loc "#{:a}")
  ``
  @[[:set {:start 0}
     [:keyword {:start 2} ":a"]]]
  ``

  (peg/match cg-capture-ast-with-start-match-and-store-loc "#'a")
  ``
  @[[:var_quote {:start 0}
     [:symbol {:start 2} "a"]]]
  ``

  (peg/match cg-capture-ast-with-start-match-and-store-loc "#_ a")
  ``
  @[[:discard {:start 0}
     [:whitespace {:start 2} " "]
     [:symbol {:start 3} "a"]]]
  ``

  (peg/match cg-capture-ast-with-start-match-and-store-loc
    "#uuid \"00000000-0000-0000-0000-000000000000\"")
  ``
  @[[:tag {:start 0}
     [:symbol {:start 1} "uuid"]
     [:whitespace {:start 5} " "]
     [:string {:start 6} "\"00000000-0000-0000-0000-000000000000\""]]]
  ``

  (peg/match cg-capture-ast-with-start-match-and-store-loc " ")
  # => @[[:whitespace {:start 0} " "]]

  (peg/match cg-capture-ast-with-start-match-and-store-loc "; hey")
  # => @[[:comment {:start 0} "; hey"]]

  (peg/match cg-capture-ast-with-start-match-and-store-loc "#! foo")
  # => @[[:comment {:start 0} "#! foo"]]

  (peg/match cg-capture-ast-with-start-match-and-store-loc "#?(:clj 0 :cljr 1)")
  ``
  @[[:conditional {:start 0}
     [:list {:start 2}
      [:keyword {:start 3} ":clj"]
      [:whitespace {:start 7} " "]
      [:number {:start 8} "0"]
      [:whitespace {:start 9} " "]
      [:keyword {:start 10} ":cljr"]
      [:whitespace {:start 15} " "]
      [:number {:start 16} "1"]]]]
  ``

  (peg/match cg-capture-ast-with-start-match-and-store-loc
    "#?@(:clj [0 1] :cljr [1 2])")
  ``
  @[[:conditional_splicing {:start 0}
     [:list {:start 3}
      [:keyword {:start 4} ":clj"]
      [:whitespace {:start 8} " "]
      [:vector {:start 9}
       [:number {:start 10} "0"]
       [:whitespace {:start 11} " "]
       [:number {:start 12} "1"]]
      [:whitespace {:start 14} " "]
      [:keyword {:start 15} ":cljr"]
      [:whitespace {:start 20} " "]
      [:vector {:start 21}
       [:number {:start 22} "1"]
       [:whitespace {:start 23} " "]
       [:number {:start 24} "2"]]]]]
  ``

  (peg/match cg-capture-ast-with-start-match-and-store-loc "##NaN")
  # => @[[:symbolic {:start 0} "NaN"]]

  (peg/match cg-capture-ast-with-start-match-and-store-loc "#=a")
  ``
  @[[:eval {:start 0}
     [:symbol {:start 2} "a"]]]
  ``

  )

(def cg-capture-ast-with-start-loc
  ~{:main (any :input)
    #
    :input (choice :whitespace
                   :comment
                   :discard
                   :form)
    #
    :form (choice :reader_macro
                  :collection
                  :literal)
    #
    :collection (choice :list
                        :vector
                        :map)
    #
    :list
    (cmt
      (sequence (position)
                (sequence "("
                          (capture (any :input))
                          (choice ")" (error "")))
                (position))
      ,|[:list {:start (first $&)}
               ;(slice $& 1 -3)])
    #
    :vector
    (cmt
      (sequence (position)
                (sequence "["
                          (capture (any :input))
                          (choice "]" (error "")))
                (position))
      ,|[:vector {:start (first $&)}
                 ;(slice $& 1 -3)])
    #
    :map
    (cmt
      (sequence (position)
                (sequence "{"
                          (capture (any :input))
                          (choice "}" (error "")))
                (position))
      ,|[:map {:start (first $&)}
              ;(slice $& 1 -3)])
    #
    :literal (choice :number
                     :macro_keyword
                     :keyword
                     :string
                     :character
                     :symbol)
    #
    :keyword
    (cmt
      (sequence (position)
                (capture (sequence ":"
                                   (choice "/"
                                           (sequence :keyword_head
                                                     (any :keyword_body)))))
                (position))
      ,|[:keyword {:start (first $&)}
                  (in $& 1)])
    #
    :keyword_head (choice :allowed_name_character
                          (set "#'"))
    #
    :allowed_name_character (if-not (set "\r\n\t\f ()[]{}\"@~^;`\\,:#'/")
                                    1)
    #
    :keyword_body (choice (set ":/")
                          :keyword_head)
    #
    :macro_keyword
    (cmt
      (sequence (position)
                (capture (sequence "::"
                                   :keyword_head
                                   (any :keyword_body)))
                (position))
      ,|[:macro_keyword {:start (first $&)}
                        (in $& 1)])
    #
    :string
    (cmt
      (sequence (position)
                (capture (sequence "\""
                                   (any (if-not (set "\"\\")
                                                1))
                                   (any (sequence "\\"
                                                  1
                                                  (any (if-not (set "\"\\")
                                                               1))))
                                   "\""))
                (position))
      ,|[:string {:start (first $&)}
                 (in $& 1)])
    #
    :number
    (cmt
      (sequence (position)
                (capture (sequence (opt (set "+-"))
                                   (some :digit)
                                   (choice :ratio_suffix
                                           :double_suffix
                                           :long_suffix)))
                (position))
      ,|[:number {:start (first $&)}
                  (in $& 1)])
    #
    :digit (range "09")
    #
    :double_suffix
    (sequence (sequence (opt (sequence "."
                                       (any :digit)))
                        (opt (sequence (set "eE")
                                       (opt (set "+-"))
                                       (some :digit))))
              (opt "M"))
    #
    :long_suffix
    (sequence (opt (choice (sequence (set "xX")
                                     (some (range "09" "af" "AF")))
                           (sequence (set "rR")
                                     (some (range "09" "az" "AZ")))
                           (some (range "07"))))
              (opt "N"))
    #
    :ratio_suffix (sequence "/"
                            (some :digit))
    #
    :character
    (cmt
      (sequence (position)
                (capture (sequence "\\"
                                   (choice :named_char
                                           :unicode
                                           :unicode_char)))
                (position))
      ,|[:character {:start (first $&)}
                    (in $& 1)])
    # XXX: this just matches anything...may be not what we want
    :unicode_char 1
    #
    :named_char (choice "backspace"
                        "formfeed"
                        "newline"
                        "return"
                        "space"
                        "tab")
    #
    :unicode (sequence "u"
                       (range "09" "af" "AF")
                       (range "09" "af" "AF")
                       (range "09" "af" "AF")
                       (range "09" "af" "AF"))
    #
    :symbol
    (cmt
      (sequence (position)
                (capture (sequence :name_head
                                   (any :name_body)))
                (position))
      ,|[:symbol {:start (first $&)}
                  (in $& 1)])
    #
    :name_head (choice "/"
                       :allowed_name_character)
    #
    :name_body (choice :name_head
                       (set ":#'"))
    #
    :reader_macro (choice :dispatch
                          :backtick
                          :quote
                          :unquote_splicing
                          :unquote
                          :deref
                          :metadata)
    #
    :metadata
    (cmt
      (sequence
        (position)
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
        (position))
      ,|[:metadata {:start (first $&)}
                   ;(slice $& 1 -3)])
    #
    :metadata_entry
    (cmt
      (sequence (position)
                (capture (sequence "^"
                                   (choice :map
                                           :string
                                           :macro_keyword
                                           :keyword
                                           :symbol)))
                (position))
      ,|[:metadata_entry {:start (first $&)}
                         ;(slice $& 1 -3)])
    #
    :deprecated_metadata_entry
    (cmt
      (sequence
        (position)
        (capture (sequence "#^"
                            (choice :map
                                    :string
                                    :macro_keyword
                                    :keyword
                                    :symbol)))
        (position))
      ,|[:deprecated_metadata_entry {:start (first $&)}
                                    ;(slice $& 1 -3)])
    #
    :backtick
    (cmt
      (sequence (position)
                (capture (sequence "`"
                                   (opt :whitespace)
                                   :form))
                (position))
      ,|[:backtick {:start (first $&)}
                   ;(slice $& 1 -3)])
    #
    :quote
    (cmt
      (sequence (position)
                (capture (sequence "'"
                                   (opt :whitespace)
                                   :form))
                (position))
      ,|[:quote {:start (first $&)}
                ;(slice $& 1 -3)])
    #
    :unquote
    (cmt
      (sequence (position)
                (capture (sequence "~"
                                   (opt :whitespace)
                                   :form))
                (position))
      ,|[:unquote {:start (first $&)}
                  ;(slice $& 1 -3)])
    #
    :unquote_splicing
    (cmt
      (sequence (position)
                (capture (sequence "~@"
                                   (opt :whitespace)
                                   :form))
                (position))
      ,|[:unquote_splicing {:start (first $&)}
                           ;(slice $& 1 -3)])
    #
    :deref
    (cmt
      (sequence (position)
                (capture (sequence "@"
                                   (opt :whitespace)
                                   :form))
                (position))
      ,|[:deref {:start (first $&)}
                ;(slice $& 1 -3)])
    #
    :dispatch (choice :set
                      :fn
                      :regex
                      :conditional
                      :conditional_splicing
                      :namespaced_map
                      :var_quote
                      :eval
                      :tag
                      :symbolic)
    #
    :fn (cmt
          (sequence (position)
                    (sequence "#"
                              (capture :list))
                    (position))
          ,|[:fn {:start (first $&)}
                 ;(slice $& 1 -3)])
    #
    :regex (cmt
             (sequence (position)
                       (sequence "#"
                                 (capture :string))
                       (position))
             ,|[:regex {:start (first $&)}
                       (last (in $& 1))])
    #
    :set (cmt
           (sequence (position)
                     (sequence "#{"
                               (capture (any :input))
                               (choice "}" (error "")))
                     (position))
           ,|[:set {:start (first $&)}
                   ;(slice $& 1 -3)])
    #
    :namespaced_map
    (cmt
      (sequence (position)
                (capture (sequence "#"
                                   (choice :macro_keyword
                                           :auto_resolve
                                           :keyword)
                                   (opt :whitespace)
                                   :map))
                (position))
      ,|[:namespaced_map {:start (first $&)}
                         ;(slice $& 1 -3)])
    #
    :auto_resolve
    (cmt
      (sequence (position)
                (capture "::")
                (position))
      ,(fn [& caps]
         [:auto_resolve {:start (first caps)}]))
    #
    :var_quote (cmt
                 (sequence (position)
                           (capture (sequence "#'"
                                              (opt :whitespace)
                                              :form))
                           (position))
                ,|[:var_quote {:start (first $&)}
                              ;(slice $& 1 -3)])
    #
    :discard (cmt
               (sequence (position)
                         (capture (sequence "#_"
                                            (opt (sequence (opt :whitespace)
                                                           :discard))
                                            (opt :whitespace)
                                            :form))
                         (position))
               ,|[:discard  {:start (first $&)}
                            ;(slice $& 1 -3)])
    #
    :tag (cmt
           (sequence (position)
                     (capture (sequence "#"
                                        :symbol
                                        (opt :whitespace)
                                        (choice :tag
                                                :collection
                                                :literal)))
                     (position))
           ,|[:tag {:start (first $&)}
                    ;(slice $& 1 -3)])
    #
    :conditional (cmt
                   (sequence (position)
                             (capture (sequence "#?"
                                                (opt :whitespace)
                                                :list))
                             (position))
                   ,|[:conditional {:start (first $&)}
                                   ;(slice $& 1 -3)])
    #
    :conditional_splicing
    (cmt
      (sequence (position)
                (capture (sequence "#?@"
                                   (opt :whitespace)
                                   :list))
                (position))
      ,|[:conditional_splicing {:start (first $&)}
                               ;(slice $& 1 -3)])
    #
    :symbolic (cmt
                (sequence (position)
                          (capture (sequence "##"
                                     :symbol))
                          (position))
                ,|[:symbolic {:start (first $&)}
                             (last ;(slice $& 1 -3))])
    #
    :eval (cmt
            (sequence (position)
                      (capture (sequence "#="
                                         (opt :whitespace)
                                         (choice :list
                                                 :symbol)))
                      (position))
            ,|[:eval {:start (first $&)}
                     ;(slice $& 1 -3)])
    #
    :whitespace (cmt
                  (sequence (position)
                            (capture (some (set "\f\n\r\t, ")))
                            (position))
                  ,|[:whitespace {:start (first $&)}
                                 (in $& 1)])
    #
    :comment (cmt (sequence (position)
                            (capture (sequence (choice ";"
                                                       "#!")
                                               (any (if-not (set "\r\n")
                                                            1))))
                            (position))
                  ,|[:comment {:start (first $&)}
                              (in $& 1)])
    })

(def cg-capture-ast-dont-store-loc-single
  ~{:main (any :input)
    #
    :input (choice :whitespace
                   :comment
                   :discard
                   :form)
    #
    :form (choice :reader_macro
                  :collection
                  :literal)
    #
    :collection (choice :list
                        :vector
                        :map)
    #
    :list
    (cmt
      (sequence (position)
                (sequence "("
                          (capture (any :input))
                          (choice ")" (error ""))))
      ,|[:list ;(slice $& 1 -3)])
    #
    :vector
    (cmt
      (sequence (position)
                (sequence "["
                          (capture (any :input))
                          (choice "]" (error ""))))
      ,|[:vector ;(slice $& 1 -3)])
    #
    :map
    (cmt
      (sequence (position)
                (sequence "{"
                          (capture (any :input))
                          (choice "}" (error ""))))
      ,|[:map ;(slice $& 1 -3)])
    #
    :literal (choice :number
                     :macro_keyword
                     :keyword
                     :string
                     :character
                     :symbol)
    #
    :keyword
    (cmt
      (sequence (position)
                (capture (sequence ":"
                                   (choice "/"
                                           (sequence :keyword_head
                                                     (any :keyword_body))))))
      ,|[:keyword (in $& 1)])
    #
    :keyword_head (choice :allowed_name_character
                          (set "#'"))
    #
    :allowed_name_character (if-not (set "\r\n\t\f ()[]{}\"@~^;`\\,:#'/")
                                    1)
    #
    :keyword_body (choice (set ":/")
                          :keyword_head)
    #
    :macro_keyword
    (cmt
      (sequence (position)
                (capture (sequence "::"
                                   :keyword_head
                                   (any :keyword_body))))
      ,|[:macro_keyword (in $& 1)])
    #
    :string
    (cmt
      (sequence (position)
                (capture (sequence "\""
                                   (any (if-not (set "\"\\")
                                                1))
                                   (any (sequence "\\"
                                                  1
                                                  (any (if-not (set "\"\\")
                                                               1))))
                                   "\"")))
      ,|[:string (in $& 1)])
    #
    :number
    (cmt
      (sequence (position)
                (capture (sequence (opt (set "+-"))
                                   (some :digit)
                                   (choice :ratio_suffix
                                           :double_suffix
                                           :long_suffix))))
      ,|[:number (in $& 1)])
    #
    :digit (range "09")
    #
    :double_suffix
    (sequence (sequence (opt (sequence "."
                                       (any :digit)))
                        (opt (sequence (set "eE")
                                       (opt (set "+-"))
                                       (some :digit))))
              (opt "M"))
    #
    :long_suffix
    (sequence (opt (choice (sequence (set "xX")
                                     (some (range "09" "af" "AF")))
                           (sequence (set "rR")
                                     (some (range "09" "az" "AZ")))
                           (some (range "07"))))
              (opt "N"))
    #
    :ratio_suffix (sequence "/"
                            (some :digit))
    #
    :character
    (cmt
      (sequence (position)
                (capture (sequence "\\"
                                   (choice :named_char
                                           :unicode
                                           :unicode_char))))
      ,|[:character (in $& 1)])
    # XXX: this just matches anything...may be not what we want
    :unicode_char 1
    #
    :named_char (choice "backspace"
                        "formfeed"
                        "newline"
                        "return"
                        "space"
                        "tab")
    #
    :unicode (sequence "u"
                       (range "09" "af" "AF")
                       (range "09" "af" "AF")
                       (range "09" "af" "AF")
                       (range "09" "af" "AF"))
    #
    :symbol
    (cmt
      (sequence (position)
                (capture (sequence :name_head
                                   (any :name_body))))
      ,|[:symbol (in $& 1)])
    #
    :name_head (choice "/"
                       :allowed_name_character)
    #
    :name_body (choice :name_head
                       (set ":#'"))
    #
    :reader_macro (choice :dispatch
                          :backtick
                          :quote
                          :unquote_splicing
                          :unquote
                          :deref
                          :metadata)
    #
    :metadata
    (cmt
      (sequence
        (position)
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
                            :symbol))))
      ,|[:metadata ;(slice $& 1 -3)])
    #
    :metadata_entry
    (cmt
      (sequence (position)
                (capture (sequence "^"
                                   (choice :map
                                           :string
                                           :macro_keyword
                                           :keyword
                                           :symbol))))
      ,|[:metadata_entry ;(slice $& 1 -3)])
    #
    :deprecated_metadata_entry
    (cmt
      (sequence
        (position)
        (capture (sequence "#^"
                            (choice :map
                                    :string
                                    :macro_keyword
                                    :keyword
                                    :symbol))))
      ,|[:deprecated_metadata_entry ;(slice $& 1 -3)])
    #
    :backtick
    (cmt
      (sequence (position)
                (capture (sequence "`"
                                   (opt :whitespace)
                                   :form)))
      ,|[:backtick ;(slice $& 1 -3)])
    #
    :quote
    (cmt
      (sequence (position)
                (capture (sequence "'"
                                   (opt :whitespace)
                                   :form)))
      ,|[:quote ;(slice $& 1 -3)])
    #
    :unquote
    (cmt
      (sequence (position)
                (capture (sequence "~"
                                   (opt :whitespace)
                                   :form)))
      ,|[:unquote ;(slice $& 1 -3)])
    #
    :unquote_splicing
    (cmt
      (sequence (position)
                (capture (sequence "~@"
                                   (opt :whitespace)
                                   :form)))
      ,|[:unquote_splicing ;(slice $& 1 -3)])
    #
    :deref
    (cmt
      (sequence (position)
                (capture (sequence "@"
                                   (opt :whitespace)
                                   :form)))
      ,|[:deref ;(slice $& 1 -3)])
    #
    :dispatch (choice :set
                      :fn
                      :regex
                      :conditional
                      :conditional_splicing
                      :namespaced_map
                      :var_quote
                      :eval
                      :tag
                      :symbolic)
    #
    :fn (cmt
          (sequence (position)
                    (sequence "#"
                              (capture :list)))
          ,|[:fn ;(slice $& 1 -3)])
    #
    :regex (cmt
             (sequence (position)
                       (sequence "#"
                                 (capture :string)))
             ,|[:regex (last (in $& 1))])
    #
    :set (cmt
           (sequence (position)
                     (sequence "#{"
                               (capture (any :input))
                               (choice "}" (error ""))))
           ,|[:set ;(slice $& 1 -3)])
    #
    :namespaced_map
    (cmt
      (sequence (position)
                (capture (sequence "#"
                                   (choice :macro_keyword
                                           :auto_resolve
                                           :keyword)
                                   (opt :whitespace)
                                   :map)))
      ,|[:namespaced_map ;(slice $& 1 -3)])
    #
    :auto_resolve
    (cmt
      (sequence (position)
                (capture "::"))
      ,(fn [& caps]
         [:auto_resolve]))
    #
    :var_quote (cmt (sequence (position)
                              (capture (sequence "#'"
                                                 (opt :whitespace)
                                                 :form)))
                    ,|[:var_quote ;(slice $& 0 -2)])
    #
    :discard (cmt (sequence (position)
                            (capture (sequence "#_"
                                               (opt (sequence (opt :whitespace)
                                                              :discard))
                                               (opt :whitespace)
                                               :form)))
                  ,|[:discard ;(slice $& 0 -2)])
    #
    :tag (cmt
           (sequence (position)
                     (capture (sequence "#"
                                        :symbol
                                        (opt :whitespace)
                                        (choice :tag
                                                :collection
                                                :literal))))
           ,|[:tag ;(slice $& 1 -3)])
    #
    :conditional (cmt
                   (sequence (position)
                             (capture (sequence "#?"
                                                (opt :whitespace)
                                                :list)))
                   ,|[:conditional ;(slice $& 1 -3)])
    #
    :conditional_splicing
    (cmt
      (sequence (position)
                (capture (sequence "#?@"
                                   (opt :whitespace)
                                   :list)))
      ,|[:conditional_splicing ;(slice $& 1 -3)])
    #
    :symbolic (cmt
                (sequence (position)
                          (capture (sequence "##"
                                     :symbol)))
                ,|[:symbolic (last ;(slice $& 1 -3))])
    #
    :eval (cmt
            (sequence (position)
                      (capture (sequence "#="
                                         (opt :whitespace)
                                         (choice :list
                                                 :symbol))))
            ,|[:eval ;(slice $& 1 -3)])
    #
    :whitespace (cmt
                  (sequence (position)
                            (capture (some (set "\f\n\r\t, "))))
                  ,|[:whitespace (in $& 1)])
    #
    :comment (cmt (sequence (position)
                            (capture (sequence (choice ";"
                                                       "#!")
                                               (any (if-not (set "\r\n")
                                                            1)))))
                  ,|[:comment (in $& 1)])
    })

(def cg-capture-ast-with-start-loc-as-number
  ~{:main (any :input)
    #
    :input (choice :whitespace
                   :comment
                   :discard
                   :form)
    #
    :form (choice :reader_macro
                  :collection
                  :literal)
    #
    :collection (choice :list
                        :vector
                        :map)
    #
    :list
    (cmt
      (sequence (position)
                (sequence "("
                          (capture (any :input))
                          (choice ")" (error ""))))
      ,|[:list (first $&)
               ;(slice $& 1 -3)])
    #
    :vector
    (cmt
      (sequence (position)
                (sequence "["
                          (capture (any :input))
                          (choice "]" (error ""))))
      ,|[:vector (first $&)
                 ;(slice $& 1 -3)])
    #
    :map
    (cmt
      (sequence (position)
                (sequence "{"
                          (capture (any :input))
                          (choice "}" (error ""))))
      ,|[:map (first $&)
              ;(slice $& 1 -3)])
    #
    :literal (choice :number
                     :macro_keyword
                     :keyword
                     :string
                     :character
                     :symbol)
    #
    :keyword
    (cmt
      (sequence (position)
                (capture (sequence ":"
                                   (choice "/"
                                           (sequence :keyword_head
                                                     (any :keyword_body))))))
      ,|[:keyword (first $&)
                  (in $& 1)])
    #
    :keyword_head (choice :allowed_name_character
                          (set "#'"))
    #
    :allowed_name_character (if-not (set "\r\n\t\f ()[]{}\"@~^;`\\,:#'/")
                                    1)
    #
    :keyword_body (choice (set ":/")
                          :keyword_head)
    #
    :macro_keyword
    (cmt
      (sequence (position)
                (capture (sequence "::"
                                   :keyword_head
                                   (any :keyword_body))))
      ,|[:macro_keyword (first $&)
                        (in $& 1)])
    #
    :string
    (cmt
      (sequence (position)
                (capture (sequence "\""
                                   (any (if-not (set "\"\\")
                                                1))
                                   (any (sequence "\\"
                                                  1
                                                  (any (if-not (set "\"\\")
                                                               1))))
                                   "\"")))
      ,|[:string (first $&)
                 (in $& 1)])
    #
    :number
    (cmt
      (sequence (position)
                (capture (sequence (opt (set "+-"))
                                   (some :digit)
                                   (choice :ratio_suffix
                                           :double_suffix
                                           :long_suffix))))
      ,|[:number (first $&)
                 (in $& 1)])
    #
    :digit (range "09")
    #
    :double_suffix
    (sequence (sequence (opt (sequence "."
                                       (any :digit)))
                        (opt (sequence (set "eE")
                                       (opt (set "+-"))
                                       (some :digit))))
              (opt "M"))
    #
    :long_suffix
    (sequence (opt (choice (sequence (set "xX")
                                     (some (range "09" "af" "AF")))
                           (sequence (set "rR")
                                     (some (range "09" "az" "AZ")))
                           (some (range "07"))))
              (opt "N"))
    #
    :ratio_suffix (sequence "/"
                            (some :digit))
    #
    :character
    (cmt
      (sequence (position)
                (capture (sequence "\\"
                                   (choice :named_char
                                           :unicode
                                           :unicode_char))))
      ,|[:character (first $&)
                    (in $& 1)])
    # XXX: this just matches anything...may be not what we want
    :unicode_char 1
    #
    :named_char (choice "backspace"
                        "formfeed"
                        "newline"
                        "return"
                        "space"
                        "tab")
    #
    :unicode (sequence "u"
                       (range "09" "af" "AF")
                       (range "09" "af" "AF")
                       (range "09" "af" "AF")
                       (range "09" "af" "AF"))
    #
    :symbol
    (cmt
      (sequence (position)
                (capture (sequence :name_head
                                   (any :name_body))))
      ,|[:symbol (first $&)
                 (in $& 1)])
    #
    :name_head (choice "/"
                       :allowed_name_character)
    #
    :name_body (choice :name_head
                       (set ":#'"))
    #
    :reader_macro (choice :dispatch
                          :backtick
                          :quote
                          :unquote_splicing
                          :unquote
                          :deref
                          :metadata)
    #
    :metadata
    (cmt
      (sequence
        (position)
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
                            :symbol))))
      ,|[:metadata (first $&)
                   ;(slice $& 1 -3)])
    #
    :metadata_entry
    (cmt
      (sequence (position)
                (capture (sequence "^"
                                   (choice :map
                                           :string
                                           :macro_keyword
                                           :keyword
                                           :symbol))))
      ,|[:metadata_entry (first $&)
                         ;(slice $& 1 -3)])
    #
    :deprecated_metadata_entry
    (cmt
      (sequence
        (position)
        (capture (sequence "#^"
                            (choice :map
                                    :string
                                    :macro_keyword
                                    :keyword
                                    :symbol))))
      ,|[:deprecated_metadata_entry (first $&)
                                    ;(slice $& 1 -3)])
    #
    :backtick
    (cmt
      (sequence (position)
                (capture (sequence "`"
                                   (opt :whitespace)
                                   :form)))
      ,|[:backtick (first $&)
                   ;(slice $& 1 -3)])
    #
    :quote
    (cmt
      (sequence (position)
                (capture (sequence "'"
                                   (opt :whitespace)
                                   :form)))
      ,|[:quote (first $&)
                ;(slice $& 1 -3)])
    #
    :unquote
    (cmt
      (sequence (position)
                (capture (sequence "~"
                                   (opt :whitespace)
                                   :form)))
      ,|[:unquote (first $&)
                  ;(slice $& 1 -3)])
    #
    :unquote_splicing
    (cmt
      (sequence (position)
                (capture (sequence "~@"
                                   (opt :whitespace)
                                   :form)))
      ,|[:unquote_splicing (first $&)
                           ;(slice $& 1 -3)])
    #
    :deref
    (cmt
      (sequence (position)
                (capture (sequence "@"
                                   (opt :whitespace)
                                   :form)))
      ,|[:deref (first $&)
                ;(slice $& 1 -3)])
    #
    :dispatch (choice :set
                      :fn
                      :regex
                      :conditional
                      :conditional_splicing
                      :namespaced_map
                      :var_quote
                      :eval
                      :tag
                      :symbolic)
    #
    :fn (cmt
          (sequence (position)
                    (sequence "#"
                              (capture :list)))
          ,|[:fn (first $&)
                 ;(slice $& 1 -3)])
    #
    :regex (cmt
             (sequence (position)
                       (sequence "#"
                                 (capture :string)))
             ,|[:regex (first $&)
                       (last (in $& 1))])
    #
    :set (cmt
           (sequence (position)
                     (sequence "#{"
                               (capture (any :input))
                               (choice "}" (error ""))))
           ,|[:set (first $&)
                   ;(slice $& 1 -3)])
    #
    :namespaced_map
    (cmt
      (sequence (position)
                (capture (sequence "#"
                                   (choice :macro_keyword
                                           :auto_resolve
                                           :keyword)
                                   (opt :whitespace)
                                   :map)))
      ,|[:namespaced_map (first $&)
                         ;(slice $& 1 -3)])
    #
    :auto_resolve
    (cmt
      (sequence (position)
                (capture "::"))
      ,(fn [& caps]
         [:auto_resolve (first caps)]))
    #
    :var_quote (cmt
                 (sequence (position)
                           (capture (sequence "#'"
                                              (opt :whitespace)
                                              :form)))
                ,|[:var_quote (first $&)
                              ;(slice $& 1 -3)])
    #
    :discard (cmt
               (sequence (position)
                         (capture (sequence "#_"
                                            (opt (sequence (opt :whitespace)
                                                           :discard))
                                            (opt :whitespace)
                                            :form)))
               ,|[:discard  (first $&)
                            ;(slice $& 1 -3)])
    #
    :tag (cmt
           (sequence (position)
                     (capture (sequence "#"
                                        :symbol
                                        (opt :whitespace)
                                        (choice :tag
                                                :collection
                                                :literal))))
           ,|[:tag (first $&)
                   ;(slice $& 1 -3)])
    #
    :conditional (cmt
                   (sequence (position)
                             (capture (sequence "#?"
                                                (opt :whitespace)
                                                :list)))
                   ,|[:conditional (first $&)
                                   ;(slice $& 1 -3)])
    #
    :conditional_splicing
    (cmt
      (sequence (position)
                (capture (sequence "#?@"
                                   (opt :whitespace)
                                   :list)))
      ,|[:conditional_splicing (first $&)
                               ;(slice $& 1 -3)])
    #
    :symbolic (cmt
                (sequence (position)
                          (capture (sequence "##"
                                     :symbol)))
                ,|[:symbolic (first $&)
                             (last ;(slice $& 1 -3))])
    #
    :eval (cmt
            (sequence (position)
                      (capture (sequence "#="
                                         (opt :whitespace)
                                         (choice :list
                                                 :symbol))))
            ,|[:eval (first $&)
                     ;(slice $& 1 -3)])
    #
    :whitespace (cmt
                  (sequence (position)
                            (capture (some (set "\f\n\r\t, "))))
                  ,|[:whitespace (first $&)
                                 (in $& 1)])
    #
    :comment (cmt (sequence (position)
                            (capture (sequence (choice ";"
                                                       "#!")
                                               (any (if-not (set "\r\n")
                                                            1)))))
                  ,|[:comment (first $&)
                              (in $& 1)])
    })

(def cg-capture-ast-with-start-loc-store-empty-struct
  ~{:main (any :input)
    #
    :input (choice :whitespace
                   :comment
                   :discard
                   :form)
    #
    :form (choice :reader_macro
                  :collection
                  :literal)
    #
    :collection (choice :list
                        :vector
                        :map)
    #
    :list
    (cmt
      (sequence (position)
                (sequence "("
                          (capture (any :input))
                          (choice ")" (error ""))))
      ,|[:list {}
               ;(slice $& 1 -3)])
    #
    :vector
    (cmt
      (sequence (position)
                (sequence "["
                          (capture (any :input))
                          (choice "]" (error ""))))
      ,|[:vector {}
                 ;(slice $& 1 -3)])
    #
    :map
    (cmt
      (sequence (position)
                (sequence "{"
                          (capture (any :input))
                          (choice "}" (error ""))))
      ,|[:map {}
              ;(slice $& 1 -3)])
    #
    :literal (choice :number
                     :macro_keyword
                     :keyword
                     :string
                     :character
                     :symbol)
    #
    :keyword
    (cmt
      (sequence (position)
                (capture (sequence ":"
                                   (choice "/"
                                           (sequence :keyword_head
                                                     (any :keyword_body))))))
      ,|[:keyword {}
                  (in $& 1)])
    #
    :keyword_head (choice :allowed_name_character
                          (set "#'"))
    #
    :allowed_name_character (if-not (set "\r\n\t\f ()[]{}\"@~^;`\\,:#'/")
                                    1)
    #
    :keyword_body (choice (set ":/")
                          :keyword_head)
    #
    :macro_keyword
    (cmt
      (sequence (position)
                (capture (sequence "::"
                                   :keyword_head
                                   (any :keyword_body))))
      ,|[:macro_keyword {}
                        (in $& 1)])
    #
    :string
    (cmt
      (sequence (position)
                (capture (sequence "\""
                                   (any (if-not (set "\"\\")
                                                1))
                                   (any (sequence "\\"
                                                  1
                                                  (any (if-not (set "\"\\")
                                                               1))))
                                   "\"")))
      ,|[:string {}
                 (in $& 1)])
    #
    :number
    (cmt
      (sequence (position)
                (capture (sequence (opt (set "+-"))
                                   (some :digit)
                                   (choice :ratio_suffix
                                           :double_suffix
                                           :long_suffix))))
      ,|[:number {}
                 (in $& 1)])
    #
    :digit (range "09")
    #
    :double_suffix
    (sequence (sequence (opt (sequence "."
                                       (any :digit)))
                        (opt (sequence (set "eE")
                                       (opt (set "+-"))
                                       (some :digit))))
              (opt "M"))
    #
    :long_suffix
    (sequence (opt (choice (sequence (set "xX")
                                     (some (range "09" "af" "AF")))
                           (sequence (set "rR")
                                     (some (range "09" "az" "AZ")))
                           (some (range "07"))))
              (opt "N"))
    #
    :ratio_suffix (sequence "/"
                            (some :digit))
    #
    :character
    (cmt
      (sequence (position)
                (capture (sequence "\\"
                                   (choice :named_char
                                           :unicode
                                           :unicode_char))))
      ,|[:character {}
                    (in $& 1)])
    # XXX: this just matches anything...may be not what we want
    :unicode_char 1
    #
    :named_char (choice "backspace"
                        "formfeed"
                        "newline"
                        "return"
                        "space"
                        "tab")
    #
    :unicode (sequence "u"
                       (range "09" "af" "AF")
                       (range "09" "af" "AF")
                       (range "09" "af" "AF")
                       (range "09" "af" "AF"))
    #
    :symbol
    (cmt
      (sequence (position)
                (capture (sequence :name_head
                                   (any :name_body))))
      ,|[:symbol {}
                 (in $& 1)])
    #
    :name_head (choice "/"
                       :allowed_name_character)
    #
    :name_body (choice :name_head
                       (set ":#'"))
    #
    :reader_macro (choice :dispatch
                          :backtick
                          :quote
                          :unquote_splicing
                          :unquote
                          :deref
                          :metadata)
    #
    :metadata
    (cmt
      (sequence
        (position)
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
                            :symbol))))
      ,|[:metadata {}
                   ;(slice $& 1 -3)])
    #
    :metadata_entry
    (cmt
      (sequence (position)
                (capture (sequence "^"
                                   (choice :map
                                           :string
                                           :macro_keyword
                                           :keyword
                                           :symbol))))
      ,|[:metadata_entry {}
                         ;(slice $& 1 -3)])
    #
    :deprecated_metadata_entry
    (cmt
      (sequence
        (position)
        (capture (sequence "#^"
                            (choice :map
                                    :string
                                    :macro_keyword
                                    :keyword
                                    :symbol))))
      ,|[:deprecated_metadata_entry {}
                                    ;(slice $& 1 -3)])
    #
    :backtick
    (cmt
      (sequence (position)
                (capture (sequence "`"
                                   (opt :whitespace)
                                   :form)))
      ,|[:backtick {}
                   ;(slice $& 1 -3)])
    #
    :quote
    (cmt
      (sequence (position)
                (capture (sequence "'"
                                   (opt :whitespace)
                                   :form)))
      ,|[:quote {}
                ;(slice $& 1 -3)])
    #
    :unquote
    (cmt
      (sequence (position)
                (capture (sequence "~"
                                   (opt :whitespace)
                                   :form)))
      ,|[:unquote {}
                  ;(slice $& 1 -3)])
    #
    :unquote_splicing
    (cmt
      (sequence (position)
                (capture (sequence "~@"
                                   (opt :whitespace)
                                   :form)))
      ,|[:unquote_splicing {}
                           ;(slice $& 1 -3)])
    #
    :deref
    (cmt
      (sequence (position)
                (capture (sequence "@"
                                   (opt :whitespace)
                                   :form)))
      ,|[:deref {}
                ;(slice $& 1 -3)])
    #
    :dispatch (choice :set
                      :fn
                      :regex
                      :conditional
                      :conditional_splicing
                      :namespaced_map
                      :var_quote
                      :eval
                      :tag
                      :symbolic)
    #
    :fn (cmt
          (sequence (position)
                    (sequence "#"
                              (capture :list)))
          ,|[:fn {}
                 ;(slice $& 1 -3)])
    #
    :regex (cmt
             (sequence (position)
                       (sequence "#"
                                 (capture :string)))
             ,|[:regex {}
                       (last (in $& 1))])
    #
    :set (cmt
           (sequence (position)
                     (sequence "#{"
                               (capture (any :input))
                               (choice "}" (error ""))))
           ,|[:set {}
                   ;(slice $& 1 -3)])
    #
    :namespaced_map
    (cmt
      (sequence (position)
                (capture (sequence "#"
                                   (choice :macro_keyword
                                           :auto_resolve
                                           :keyword)
                                   (opt :whitespace)
                                   :map)))
      ,|[:namespaced_map {}
                         ;(slice $& 1 -3)])
    #
    :auto_resolve
    (cmt
      (sequence (position)
                (capture "::"))
      ,(fn [& caps]
         [:auto_resolve {}]))
    #
    :var_quote (cmt
                 (sequence (position)
                           (capture (sequence "#'"
                                              (opt :whitespace)
                                              :form)))
                ,|[:var_quote {}
                              ;(slice $& 1 -3)])
    #
    :discard (cmt
               (sequence (position)
                         (capture (sequence "#_"
                                            (opt (sequence (opt :whitespace)
                                                           :discard))
                                            (opt :whitespace)
                                            :form)))
               ,|[:discard {}
                           ;(slice $& 1 -3)])
    #
    :tag (cmt
           (sequence (position)
                     (capture (sequence "#"
                                        :symbol
                                        (opt :whitespace)
                                        (choice :tag
                                                :collection
                                                :literal))))
           ,|[:tag {}
                   ;(slice $& 1 -3)])
    #
    :conditional (cmt
                   (sequence (position)
                             (capture (sequence "#?"
                                                (opt :whitespace)
                                                :list)))
                   ,|[:conditional {}
                                   ;(slice $& 1 -3)])
    #
    :conditional_splicing
    (cmt
      (sequence (position)
                (capture (sequence "#?@"
                                   (opt :whitespace)
                                   :list)))
      ,|[:conditional_splicing {}
                               ;(slice $& 1 -3)])
    #
    :symbolic (cmt
                (sequence (position)
                          (capture (sequence "##"
                                     :symbol)))
                ,|[:symbolic {}
                             (last ;(slice $& 1 -3))])
    #
    :eval (cmt
            (sequence (position)
                      (capture (sequence "#="
                                         (opt :whitespace)
                                         (choice :list
                                                 :symbol))))
            ,|[:eval {}
                     ;(slice $& 1 -3)])
    #
    :whitespace (cmt
                  (sequence (position)
                            (capture (some (set "\f\n\r\t, "))))
                  ,|[:whitespace {}
                                 (in $& 1)])
    #
    :comment (cmt (sequence (position)
                            (capture (sequence (choice ";"
                                                       "#!")
                                               (any (if-not (set "\r\n")
                                                            1)))))
                  ,|[:comment {}
                              (in $& 1)])
    })

(comment

  (comment

    (let [src
          (slurp (string (os/getenv "HOME")
                   "/src/alc.x-as-tests/src/alc/x_as_tests/main.clj"))
          almost-ast
          (peg/match cg-capture-ast src)
          ast
          (array/insert almost-ast 0 :code)]
      (= (string src)
        (code ast)))

    # round trip - 73 ms per
    (let [start (os/time)]
      (each i (range 1000)
            (let [src-str
                  (string (slurp (string (os/getenv "HOME")
                                   "/src/clojure/src/clj/clojure/core.clj")))]
              (= src-str
                (code (array/insert
                        (peg/match cg-capture-ast src-str)
                        0
                        :code)))))
      (print (- (os/time) start)))

    (defn test-peg-on-cc
      [a-peg n-iter]
      (let [start (os/time)]
        (each i (range n-iter)
              (let [src-str
                    (string
                      (slurp (string (os/getenv "HOME")
                               "/src/clojure/src/clj/clojure/core.clj")))]
                (peg/match a-peg src-str)))
        (- (os/time) start)))

    # XXX: effectively turns off gc?
    (gcsetinterval 9999999999999)
    # XXX: current default value
    #(gcsetinterval 4194304)
    (gccollect)

    # parsing - 57 ms / 35, 37 ms with gc off
    (test-peg-on-cc
      cg-capture-ast
      1000)

    # capture start but not storing locations - 61 ms / 55, 55 ms gc off
    (test-peg-on-cc
      cg-capture-ast-dont-store-loc-single
      1000)

    # capture both but not storing locations - 106 ms / 58, 57 ms gc off
    (test-peg-on-cc
      cg-capture-ast-dont-store-loc
      1000)

    # XXX: capture just start

    # capture just start and store as number - 66 ms per / 54, 59 ms gc off
    (test-peg-on-cc
      cg-capture-ast-with-start-loc-as-number
      1000)

    # capture just start and store empty struct - 72 ms per / 57, 59 ms gc off
    (test-peg-on-cc
      cg-capture-ast-with-start-loc-store-empty-struct
      1000)

    # capture just start and store struct - 150 ms per / 58, 61 ms gc off
    (test-peg-on-cc
      cg-capture-ast-with-start-match-and-store-loc
      1000)

    # XXX: capture both start and end

    # capture both but store only start - 152 ms per / 65 ms gc off
    (test-peg-on-cc
      cg-capture-ast-with-start-loc
      1000)

    # capture and store both locations - 157 ms / 71, 66 ms gc off
    (test-peg-on-cc
      cg-capture-ast-with-loc
      1000)

    )

  )
