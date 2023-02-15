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
  # =>
  @[]

  (peg/match cg-one "(def a 1)")
  # =>
  @[]

  (try
    (peg/match cg-one "[:a :b)")
    ([e] e))
  # =>
  "missing ]"

  (peg/match cg-one "(def a ; hi\n 1)")
  # =>
  @[]

  (try
    (peg/match cg-one "(def a ; hi 1)")
    ([e] e))
  # =>
  "missing )"

  (peg/match cg-one "[1]")
  # =>
  @[]

  (peg/match cg-one "; hello")
  # =>
  @[]

  (peg/match cg-one "8")
  # =>
  @[]

  (peg/match cg-one "[:a :b]")
  # =>
  @[]

  (peg/match cg-one "[:a :b] 1")
  # =>
  @[]

 )

(def cg-capture-all
  (->
   # cg is a struct, need something mutable
   (table ;(kvs cg))
   # capture all
   (put :main ~(capture ,(in cg :main)))
   # tried using a table with a peg but had a problem, so use a struct
   table/to-struct))

(comment

 (peg/match cg-capture-all "")
  # =>
  nil

 (peg/match cg-capture-all "()")
  # =>
  @["()"]

 (peg/match cg-capture-all "[]")
  # =>
  @["[]"]

 (peg/match cg-capture-all "{}")
  # =>
  @["{}"]

 (peg/match cg-capture-all "#{}")
  # =>
  @["#{}"]

 (peg/match cg-capture-all "#::{}")
  # =>
  @["#::{}"]

 (peg/match cg-capture-all "1")
  # =>
  @["1"]

 (peg/match cg-capture-all "1/2")
  # =>
  @["1/2"]

 (peg/match cg-capture-all ":a")
  # =>
  @[":a"]

 (peg/match cg-capture-all "::a")
  # =>
  @["::a"]

 (peg/match cg-capture-all "\"a\"")
  # =>
  @["\"a\""]

 (peg/match cg-capture-all "#\".\"")
  # =>
  @["#\".\""]

 (peg/match cg-capture-all "#_ a")
  # =>
  @["#_ a"]

 (peg/match cg-capture-all "(def a 1) :a")
  # =>
  @["(def a 1) :a"]

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
  # =>
  @["\n"]

 (peg/match cg-capture-one sample-source 13)
  # =>
  @["(+ 1 1)"]

 (peg/match cg-capture-one sample-source 21)
  # =>
  @[";; => 2"]

 )

(def cg-capture-top-levels
  (->
   # cg is a struct, need something mutable
   (table ;(kvs cg))
   # capture each top-level
   (put :main '(some (capture :input)))
   # tried using a table with a peg but had a problem, so use a struct
   table/to-struct))

(comment

 (peg/match cg-capture-top-levels "")
  # =>
  nil

 (peg/match cg-capture-top-levels "()")
  # =>
  @["()"]

 (peg/match cg-capture-top-levels "[]")
  # =>
  @["[]"]

 (peg/match cg-capture-top-levels "{}")
  # =>
  @["{}"]

 (peg/match cg-capture-top-levels "#{}")
  # =>
  @["#{}"]

 (peg/match cg-capture-top-levels "#::{}")
  # =>
  @["#::{}"]

 (peg/match cg-capture-top-levels "1")
  # =>
  @["1"]

 (peg/match cg-capture-top-levels "1/2")
  # =>
  @["1/2"]

 (peg/match cg-capture-top-levels ":a")
  # =>
  @[":a"]

 (peg/match cg-capture-top-levels "::a")
  # =>
  @["::a"]

 (peg/match cg-capture-top-levels "\"a\"")
  # =>
  @["\"a\""]

 (peg/match cg-capture-top-levels "#\".\"")
  # =>
  @["#\".\""]

 (peg/match cg-capture-top-levels "#_ a")
  # =>
  @["#_ a"]

 (peg/match cg-capture-top-levels "(def a 1) :a")
  # =>
  @["(def a 1)" " " ":a"]

 (peg/match cg-capture-top-levels "^{:a true} [:a :b]")
  # =>
  @["^{:a true} [:a :b]"]

 (peg/match cg-capture-top-levels "\\a")
  # =>
  @["\\a"]

 )
