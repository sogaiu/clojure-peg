(def cg
  ~{:main (some :input)
    #
    :input (choice :non-form
                   :form)
    #
    :non-form (choice :whitespace
                      :comment
                      :discard)
    #
    :whitespace (some (set "\f\n\r\t, "))
    #
    :comment (sequence (choice ";"
                               "#!")
                       (any (if-not (set "\r\n")
                                    1)))
    #
    :discard (sequence "#_"
                       (opt (sequence (any (choice :comment
                                                   :whitespace))
                                      :discard))
                       (any (choice :comment
                                    :whitespace))
                       :form)
    #
    :form (choice :reader-macro
                  :collection
                  :literal)
    #
    :reader-macro (choice :dispatch
                          :backtick
                          :quote
                          :unquote-splicing
                          :unquote
                          :deref
                          :metadata)
    #
    :dispatch (choice :set
                      :fn
                      :regex
                      :conditional
                      :conditional-splicing
                      :namespaced-map
                      :var-quote
                      :eval
                      :tag
                      :symbolic)
    #
    :set (sequence "#{"
                   (any :input)
                   (choice "}" (error "")))
    #
    :fn (sequence "#" :list)
    #
    :regex (sequence "#" :string)
    #
    :namespaced-map (sequence "#"
                              (choice :macro-keyword
                                      :auto-resolve
                                      :keyword)
                              (any :non-form)
                              :map)
    #
    :conditional (sequence "#?"
                           (any :whitespace)
                           :list)
    #
    :conditional-splicing (sequence "#?@"
                                    (any :whitespace)
                                    :list)
    #
    :auto-resolve "::"
    #
    :var-quote (sequence "#'"
                         (any :non-form)
                         :form)
    #
    :eval (sequence "#="
                    (any :non-form)
                    (choice :list
                            :symbol))
    #
    :tag (sequence "#"
                   :symbol
                   (any :non-form)
                   (choice :tag
                           :collection
                           :literal))
    #
    :symbolic (sequence "##"
                        (any :non-form)
                        :symbol)
    #
    :backtick (sequence "`"
                        (any :non-form)
                        :form)
    #
    :quote (sequence "'"
                     (any :non-form)
                     :form)
    #
    :unquote (sequence "~"
                       (any :non-form)
                       :form)
    #
    :unquote-splicing (sequence "~@"
                                (any :non-form)
                                :form)
    #
    :deref (sequence "@"
                    (any :non-form)
                    :form)
    #
    :metadata
    (sequence (some (sequence (choice :metadata-entry
                                      :deprecated-metadata-entry)
                              (any :non-form)))
              (choice :collection
                      :conditional
                      :namespaced-map
                      :set
                      :tag
                      :fn
                      :unquote-splicing
                      :unquote
                      :deref
                      :quote
                      :backtick
                      :var-quote
                      :symbol))
    #
    :metadata-entry (sequence "^"
                              (any :non-form)
                              (choice :conditional
                                      :map
                                      :string
                                      :macro-keyword
                                      :keyword
                                      :symbol))
    #
    :deprecated-metadata-entry (sequence "#^"
                                         (any :non-form)
                                         (choice :conditional
                                                 :map
                                                 :string
                                                 :macro-keyword
                                                 :keyword
                                                 :symbol))
    #
    :collection (choice :list
                        :vector
                        :map)
    #
    :list (sequence "("
                    (any :input)
                    (choice ")" (error "")))
    #
    :vector (sequence "["
                      (any :input)
                      (choice "]" (error "")))
    #
    :map (sequence "{"
                    (any :input)
                    (choice "}" (error "")))
    #
    :literal (choice :number
                     :macro-keyword
                     :keyword
                     :string
                     :character
                     :symbol)
    #
    :number (sequence (opt (set "+-"))
                      (choice :hex-number
                              :octal-number
                              :radix-number
                              :ratio
                              :double
                              :integer))
    #
    :double (sequence (some :digit)
                      (opt (sequence "."
                                     (any :digit)))
                      (opt (sequence (set "eE")
                                     (opt (set "+-"))
                                     (some :digit)))
                      (opt "M"))
    #
    :digit (range "09")
    #
    :integer (sequence (some :digit)
                       (opt (set "MN")))
    #
    :hex-number (sequence "0"
                          (set "xX")
                          (some :hex)
                          (opt "N"))
    #
    :hex (range "09" "af" "AF")
    #
    :octal-number (sequence "0"
                            (some :octal)
                            (opt "N"))
    #
    :octal (range "07")
    #
    :radix-number (sequence (some :digit)
                            (set "rR")
                            (some (range "09" "az" "AZ")))
    #
    :ratio (sequence (some :digit)
                     "/"
                     (some :digit))
    #
    :macro-keyword (sequence "::"
                             :keyword-head
                             (any :keyword-body))
    #
    :keyword (sequence ":"
                       (choice "/"
                               (sequence :keyword-head
                                         (any :keyword-body))))
    #
    :keyword-head (if-not (set "\f\n\r\t ()[]{}\"@~^;`\\,:/")
                          1)
    #
    :keyword-body (choice (set ":'/")
                          :keyword-head)
    #
    :string (sequence "\""
                      (any (if-not (set "\"\\")
                                   1))
                      (any (sequence "\\"
                                     1
                                     (any (if-not (set "\"\\")
                                                  1))))
                      "\"")
    #
    :character (sequence "\\"
                         (choice :named-char
                                 :octal-char
                                 :unicode
                                 :unicode-char))
    #
    :named-char (choice "backspace"
                        "formfeed"
                        "newline"
                        "return"
                        "space"
                        "tab")
    # XXX: \o477 and others are not valid
    :octal-char (sequence "o"
                          (choice [1 :octal]
                                  [2 :octal]
                                  [3 :octal]))
    #
    :unicode (sequence "u" [4 :hex])
    # XXX: this just matches anything...may be not what we want
    :unicode-char 1
    #
    :symbol (sequence :symbol-head
                      (any :symbol-body))
    #
    :symbol-head (if-not (set "\f\n\r\t ()[]{}\"@~^;`\\,:#'0123456789")
                         1)
    #
    :symbol-body (choice :symbol-head
                         (set ":#'0123456789"))
    })

(comment

  (peg/match cg "")
  # => nil

  (peg/match cg " ")
  # => @[]

  (peg/match cg ";")
  # => @[]

  (peg/match cg "; ")
  # => @[]

  (peg/match cg "1")
  # => @[]

  (peg/match cg "2.0")
  # => @[]

  (peg/match cg "6.022e23")
  # => @[]

  (peg/match cg "1e8")
  # => @[]

  (peg/match cg "1/2")
  # => @[]

  (peg/match cg "0x1")
  # => @[]

  (peg/match cg "01")
  # => @[]

  (peg/match cg "017")
  # => @[]

  (peg/match cg "0377")
  # => @[]

  (peg/match cg "2r01")
  # => @[]

  (peg/match cg "36rA")
  # => @[]

  (peg/match cg "()")
  # => @[]

  (peg/match cg "{}")
  # => @[]

  (peg/match cg "{:a 1}")
  # => @[]

  (peg/match cg "a")
  # => @[]

  (peg/match cg ":a")
  # => @[]

  (peg/match cg "::a")
  # => @[]

  (peg/match cg "#_ 2")
  # => @[]

  (peg/match cg "@a")
  # => @[]

  (peg/match cg "'a")
  # => @[]

  (peg/match cg "##Inf")
  # => @[]

  (peg/match cg "## NaN")
  # => @[]

  (peg/match cg "#'a")
  # => @[]

  (peg/match cg "#::{}")
  # => @[]

  (peg/match cg "#{}")
  # => @[]

  (peg/match cg "(defn hi [] 1)")
  # => @[]

  (peg/match cg "^{:a true} [:a :b]")
  # => @[]

  (peg/match cg "#?(:clj 1 :cljr 2)")
  # => @[]

  (peg/match cg "#? (:clj 1 :cljr 2)")
  # => @[]

  (peg/match cg "[#?@(:clj [:a] :cljr [:b])]")
  # => @[]

  (peg/match cg "[#?@ (:clj [:a] :cljr [:b])]")
  # => @[]

  )
