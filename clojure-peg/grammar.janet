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
                           (any :non-form)
                           :list)
    #
    :conditional-splicing (sequence "#?@"
                                    (any :non-form)
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
                      (some :digit)
                      (opt (choice :ratio-suffix
                                   :double-suffix
                                   :long-suffix)))
    #
    :digit (range "09")
    #
    :ratio-suffix (sequence "/"
                            (some :digit))
    #
    :double-suffix
    (sequence (sequence (opt (sequence "."
                                       (any :digit)))
                        (opt (sequence (set "eE")
                                       (opt (set "+-"))
                                       (some :digit))))
              (opt "M"))
    #
    :long-suffix
    (sequence (opt (choice (sequence (set "xX")
                                     (some (range "09" "af" "AF")))
                           (sequence (set "rR")
                                     (some (range "09" "az" "AZ")))
                           (some (range "07"))))
              (opt "N"))
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
    :keyword-head (choice :allowed-name-character
                          (set "#'"))
    #
    :allowed-name-character
    (if-not (set "\f\n\r\t ()[]{}\"@~^;`\\,:#'/")
            1)
    #
    :keyword-body (choice (set ":/")
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
                                 :unicode
                                 :unicode-char))
    #
    :named-char (choice "backspace"
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
    # XXX: this just matches anything...may be not what we want
    :unicode-char 1
    #
    :symbol (sequence :name-head
                      (any :name-body))
    #
    :name-head (choice "/"
                       :allowed-name-character)
    #
    :name-body (choice :name-head
                       (set ":#'"))
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

  )
