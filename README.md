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

With Location Info
```janet
(import clojure-peg/location)

# parse code string
(location/par "(+ 1 1)")
# =>
'@[:code @{:bc 1 :bl 1
           :ec 8 :el 1}
   (:list @{:bc 1 :bl 1
            :ec 8 :el 1}
          (:symbol @{:bc 2 :bl 1
                     :ec 3 :el 1} "+")
          (:whitespace @{:bc 3 :bl 1
                         :ec 4 :el 1} " ")
          (:number @{:bc 4 :bl 1
                     :ec 5 :el 1} "1")
          (:whitespace @{:bc 5 :bl 1
                         :ec 6 :el 1} " ")
          (:number @{:bc 6 :bl 1
                     :ec 7 :el 1} "1"))]

# generate code string
(location/gen
  '@[:code @{}
     (:list @{}
            (:symbol @{} "+")
            (:whitespace @{} " ")
            (:number @{} "1")
            (:whitespace @{} " ")
            (:number @{} "1"))])
# =>
"(+ 1 1)"

# roundtrip
(def src "{:x  :y \n :z  [:a  :b    :c]}")

(= (location/gen (location/par src))
   src)
# =>
true
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
