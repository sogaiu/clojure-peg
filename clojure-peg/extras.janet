(import ./grammar :prefix "")

# make a version of cg that matches a single form
(def cg-one
  # just recognize one form
  (put (table ;(kvs cg))
       :main :input))

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
  # capture all
  (put (table ;(kvs cg))
       :main
       ~(capture ,(in cg :main))))

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
  # capture one
  (put (table ;(kvs cg))
       :main '(capture :input)))

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
   # capture each top-level
   (put (table ;(kvs cg))
        :main '(some (capture :input))))

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
