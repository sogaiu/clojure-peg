# clojure-peg

Some code for parsing and generating Clojure source code...using
Janet's Parsing Expression Grammar support.

## Usage Examples

Basic Parsing and Generation
```janet
(import clojure-peg/rewrite)

# parse code string
(rewrite/par "(+ 1 1)")
# =>
'@[:code
   (:list
     (:symbol "+") (:whitespace " ")
     (:number "1") (:whitespace " ")
     (:number "1"))]

# generate code string
(rewrite/gen
  '@(:map
     (:keyword ":a") (:whitespace " ")
     (:number "1")))
# =>
"{:a 1}"

# roundtrip
(def src "{:x  :y \n :z  [:a  :b    :c]}")

(= (rewrite/gen (rewrite/par src))
   src)
# =>
true

# replace underscores in keywords with dashes
(def src-2
  "(defn a [] {:a_1 1 :b_2 2})")

(rewrite/gen
  (postwalk |(if (and (tuple? $)
                      (= (first $) :keyword)
                      (string/find "_" (in $ 1)))
               (tuple ;(let [arr (array ;$)]
                         (put arr 1
                              (string/replace-all "_" "-" (in $ 1)))))
               $)
            (rewrite/par src-2)))
# =>
"(defn a [] {:a-1 1 :b-2 2})"
```

## Roundtrip Testing

To perform roundtrip testing on syntactically valid `.clj` files, use
the `test-samples.janet` script in the `support` directory.

For example, to test all `.clj` files in a directory at path `/tmp/samples`:

```
janet support/test-samples.janet /tmp/samples
```

Individual syntactically valid `.clj` files may also be tested by
specifying file paths.

For example, to test `sample.clj` that lives under `/tmp`:

```
janet support/test-samples.janet /tmp/sample.clj
```
