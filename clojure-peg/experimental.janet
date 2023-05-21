(import ./grammar :prefix "")

# XXX: capture and store the start position as a number
(def cg-capture-ast
  (let [ca (table ;(kvs cg))]
    (each kwd [:character :comment :keyword :macro-keyword :number
               :string :symbol :whitespace]
          (put ca kwd
               ~(cmt (sequence (position)
                               (capture ,(in ca kwd)))
                    ,|[kwd (first $&)
                           (in $& 1)])))
    (each kwd [:backtick :conditional :conditional-splicing
               :deprecated-metadata-entry :deref :discard
               :eval :metadata :metadata-entry :namespaced-map
               :quote :tag :unquote :unquote-splicing :var-quote]
          (put ca kwd
               ~(cmt (sequence (position)
                               (capture ,(in ca kwd)))
                     ,|[kwd (first $&)
                            ;(slice $& 1 -2)])))
    (each kwd [:list :map :set :vector]
          (let [original (in ca kwd)
                wrap-target (get-in ca [kwd 2])
                replacement (tuple # array needs to be converted
                              ;(put (array ;original)
                                    2 ~(capture ,wrap-target)))]
            (put ca kwd
                 ~(cmt (sequence (position)
                                 ,replacement)
                       ,|[kwd (first $&)
                              ;(slice $& 1 -2)]))))
    #
    (let [original (in ca :regex)
          wrap-target (get-in ca [:regex 2])
          replacement (tuple # array needs to be converted
                        ;(put (array ;original)
                              2 ~(capture ,wrap-target)))]
      (put ca :regex
           ~(cmt (sequence (position)
                           ,replacement)
                 ,|[:regex (first $&)
                           (last (in $& 1))])))
    #
    (let [original (in ca :fn)
          wrap-target (get-in ca [:fn 2])
          replacement (tuple # array needs to be converted
                        ;(put (array ;original)
                              2 ~(capture ,wrap-target)))]
      (put ca :fn
           ~(cmt (sequence (position)
                           ,replacement)
                 ,|[:fn (first $&)
                        ;(slice $& 1 -2)])))
    #
    (put ca :symbolic
         ~(cmt (sequence (position)
                         (capture ,(in ca :symbolic)))
               ,|[:symbolic (first $&)
                            (last ;(slice $& 1 -2))]))
    #
    (put ca :auto-resolve
         ~(cmt (sequence (position)
                         (capture ,(in ca :auto-resolve)))
               ,(fn [& caps]
                  [:auto-resolve (first caps)])))
    ca))

(comment

  (peg/match cg-capture-ast ":a")
  # =>
  @[[:keyword 0 ":a"]]

  (peg/match cg-capture-ast "\"smile\"")
  # =>
  @[[:string 0 "\"smile\""]]

  (peg/match cg-capture-ast "1/2")
  # =>
  @[[:number 0 "1/2"]]

  (peg/match cg-capture-ast "defmacro")
  # =>
  @[[:symbol 0 "defmacro"]]

  (peg/match cg-capture-ast "::a")
  # =>
  @[[:macro-keyword 0 "::a"]]

  (peg/match cg-capture-ast "\\a")
  # =>
  @[[:character 0 "\\a"]]

  (peg/match cg-capture-ast "{}")
  # =>
  @[[:map 0]]

  (deep=
    #
    (peg/match cg-capture-ast "{:a 1}")
    #
    @[[:map 0
       [:keyword 1 ":a"]
       [:whitespace 3 " "]
       [:number 4 "1"]]])
  # =>
  true

  (deep=
    #
    (peg/match cg-capture-ast "#::{}")
    #
    @[[:namespaced-map 0
       [:auto-resolve 1]
       [:map 3]]])
  # =>
  true

  (deep=
    #
    (peg/match cg-capture-ast "#::a{}")
    #
    @[[:namespaced-map 0
       [:macro-keyword 1 "::a"]
       [:map 4]]])
  # => true

  (deep=
    #
    (peg/match cg-capture-ast "#:a{}")
    #
    @[[:namespaced-map 0
       [:keyword 1 ":a"]
       [:map 3]]])
  # =>
  true

  (peg/match cg-capture-ast "[]")
  # =>
  @[[:vector 0]]

  (deep=
    #
    (peg/match cg-capture-ast "[:a]")
    #
    @[[:vector 0
       [:keyword 1 ":a"]]])
  # =>
  true

  (peg/match cg-capture-ast "()")
  #  =>
  @[[:list 0]]

  (deep=
    #
    (peg/match cg-capture-ast "(:a)")
    #
    @[[:list 0
       [:keyword 1 ":a"]]])
  # =>
  true

  (deep=
    #
    (peg/match cg-capture-ast "^{:a true} [:a]")
    #
    @[[:metadata 0
       [:metadata-entry 0
        [:map 1
         [:keyword 2 ":a"]
         [:whitespace 4 " "]
         [:symbol 5 "true"]]]
       [:whitespace 10 " "]
       [:vector 11
        [:keyword 12 ":a"]]]])
  # =>
  true

  (deep=
    #
    (peg/match cg-capture-ast "#^{:a true} [:a]")
    #
    @[[:metadata 0
       [:deprecated-metadata-entry 0
        [:map 2
         [:keyword 3 ":a"]
         [:whitespace 5 " "]
         [:symbol 6 "true"]]]
       [:whitespace 11 " "]
       [:vector 12
        [:keyword 13 ":a"]]]])
  # =>
  true

  (deep=
    #
    (peg/match cg-capture-ast "`a")
    #
    @[[:backtick 0
       [:symbol 1 "a"]]])
  # =>
  true

  (deep=
    #
    (peg/match cg-capture-ast "'a")
    #
    @[[:quote 0
       [:symbol 1 "a"]]])
  # =>
  true

  (deep=
    #
    (peg/match cg-capture-ast "~a")
    #
    @[[:unquote 0
       [:symbol 1 "a"]]])
  # =>
  true

  (deep=
    #
    (peg/match cg-capture-ast "~@a")
    #
    @[[:unquote-splicing 0
       [:symbol 2 "a"]]])
  # =>
  true

  (deep=
    #
    (peg/match cg-capture-ast "@a")
    #
    @[[:deref 0
       [:symbol 1 "a"]]])
  # =>
  true

  (deep=
    #
    (peg/match cg-capture-ast "#(inc %)")
    #
    @[[:fn 0
       [:list 1
        [:symbol 2 "inc"]
        [:whitespace 5 " "]
        [:symbol 6 "%"]]]])
  # =>
  true

  (peg/match cg-capture-ast "#\".\"")
  # =>
  @[[:regex 0 "\".\""]]

  (deep=
    #
    (peg/match cg-capture-ast "#{:a}")
    #
    @[[:set 0
       [:keyword 2 ":a"]]])
  # =>
  true

  (deep=
    #
    (peg/match cg-capture-ast "#'a")
    #
    @[[:var-quote 0
       [:symbol 2 "a"]]])
  # =>
  true

  (deep=
    #
    (peg/match cg-capture-ast "#_ a")
    #
    @[[:discard 0
       [:whitespace 2 " "]
       [:symbol 3 "a"]]])
  # =>
  true

  (deep=
    (peg/match cg-capture-ast
               "#uuid \"00000000-0000-0000-0000-000000000000\"")
    #
    @[[:tag 0
       [:symbol 1 "uuid"]
       [:whitespace 5 " "]
       [:string 6 "\"00000000-0000-0000-0000-000000000000\""]]])
  # =>
  true

  (peg/match cg-capture-ast " ")
  # =>
  @[[:whitespace 0 " "]]

  (peg/match cg-capture-ast "; hey")
  # =>
  @[[:comment 0 "; hey"]]

  (peg/match cg-capture-ast "#! foo")
  # =>
  @[[:comment 0 "#! foo"]]

  (deep=
    #
    (peg/match cg-capture-ast "#?(:clj 0 :cljr 1)")
    #
    @[[:conditional 0
       [:list 2
        [:keyword 3 ":clj"]
        [:whitespace 7 " "]
        [:number 8 "0"]
        [:whitespace 9 " "]
        [:keyword 10 ":cljr"]
        [:whitespace 15 " "]
        [:number 16 "1"]]]])
  # =>
  true

  (deep=
    #
    (peg/match cg-capture-ast
               "#?@(:clj [0 1] :cljr [1 2])")
    #
    @[[:conditional-splicing 0
       [:list 3
        [:keyword 4 ":clj"]
        [:whitespace 8 " "]
        [:vector 9
         [:number 10 "0"]
         [:whitespace 11 " "]
         [:number 12 "1"]]
        [:whitespace 14 " "]
        [:keyword 15 ":cljr"]
        [:whitespace 20 " "]
        [:vector 21
         [:number 22 "1"]
         [:whitespace 23 " "]
         [:number 24 "2"]]]]])
  # =>
  true

  (peg/match cg-capture-ast "##NaN")
  # =>
  @[[:symbolic 0 "NaN"]]

  (deep=
    #
    (peg/match cg-capture-ast "#=a")
    #
    @[[:eval 0
       [:symbol 2 "a"]]])
  # =>
  true

  )

(comment

  (comment

    (import ./rewrite)

    (import ./location)

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

    # just parsing
    #    gc on: 57, 58, 61 ms
    #   gc off: 35, 37, 37 ms
    (test-peg-on-cc rewrite/cg-capture-ast 1000)

    # capture just start and store as number
    #    gc on: 66, 68, 108, 105, 106 ms per
    #   gc off: 54, 59, 61 ms per
    (test-peg-on-cc cg-capture-ast 1000)

    # capture and store both locations
    #    gc on: 157, 158, 161 ms
    #   gc off: 71, 66, 77 ms
    (test-peg-on-cc location/cg-capture-ast 1000)

    )

  )
