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

# this looks complicated, but it's just wrapping pieces of the original
# grammar with appropriate capture constructs that also capture start and
# end positions
(def cg-capture-ast-with-loc
  # cg is a struct, need something mutable
  (let [ca (table ;(kvs cg))]
    (each kwd [:character :comment :keyword :macro_keyword :number
               :string :symbol :whitespace]
          (put ca kwd
               ~(cmt (sequence (position)
                               (capture ,(in ca kwd))
                               (position))
                     ,|[kwd {:start (first $&)
                             :end (last $&)}
                            (in $& 1)])))
    (each kwd [:backtick :conditional :conditional_splicing
               :deprecated_metadata_entry :deref :discard
               :eval :metadata :metadata_entry :namespaced_map
               :quote :tag :unquote :unquote_splicing :var_quote]
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
    (put ca :auto_resolve
         ~(cmt (sequence (position)
                         (capture ,(in ca :auto_resolve))
                         (position))
               ,(fn [& caps]
                  [:auto_resolve {:start (first caps)
                                  :end (last caps)}])))
    # tried using a table with a peg but had a problem, so use a struct
    (table/to-struct ca)))

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

(def cg-capture-ast-with-start-loc-as-number
  # cg is a struct, need something mutable
  (let [ca (table ;(kvs cg))]
    (each kwd [:character :comment :keyword :macro_keyword :number
               :string :symbol :whitespace]
          (put ca kwd
               ~(cmt (sequence (position)
                               (capture ,(in ca kwd)))
                    ,|[kwd (first $&)
                           (in $& 1)])))
    (each kwd [:backtick :conditional :conditional_splicing
               :deprecated_metadata_entry :deref :discard
               :eval :metadata :metadata_entry :namespaced_map
               :quote :tag :unquote :unquote_splicing :var_quote]
          (put ca kwd
               ~(cmt (sequence (position)
                               (capture ,(in ca kwd)))
                     ,|[kwd (first $&)
                            ;(slice $& 1 -3)])))
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
                              ;(slice $& 1 -3)]))))
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
                        ;(slice $& 1 -3)])))
    #
    (put ca :symbolic
         ~(cmt (sequence (position)
                         (capture ,(in ca :symbolic)))
               ,|[:symbolic (first $&)
                            (last ;(slice $& 1 -3))]))
    #
    (put ca :auto_resolve
         ~(cmt (sequence (position)
                         (capture ,(in ca :auto_resolve)))
               ,(fn [& caps]
                  [:auto_resolve (first caps)])))
    # tried using a table with a peg but had a problem, so use a struct
    (table/to-struct ca)))

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

    # parsing - 57, 58 ms / 35, 37 ms with gc off
    (test-peg-on-cc
      cg-capture-ast
      1000)

    # capture just start and store as number - 66, 68 ms per / 54, 59 ms gc off
    (test-peg-on-cc
      cg-capture-ast-with-start-loc-as-number
      1000)

    # capture and store both locations - 157, 158 ms / 71, 66 ms gc off
    (test-peg-on-cc
      cg-capture-ast-with-loc
      1000)

    )

  )
