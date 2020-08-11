(def cg
  ~{:main (any :input)
    #
    :input (choice :non_form
                   :form)
    #
    :non_form (choice :whitespace
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
    :form (choice :reader_macro
                  :collection
                  :literal)
    #
    :reader_macro (choice :dispatch
                          :backtick
                          :quote
                          :unquote_splicing
                          :unquote
                          :deref
                          :metadata)
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
    :set (sequence "#{"
                   (any :input)
                   (choice "}" (error "")))
    #
    :fn (sequence "#" :list)
    #
    :regex (sequence "#" :string)
    #
    :namespaced_map (sequence "#"
                              (choice :macro_keyword
                                      :auto_resolve
                                      :keyword)
                              (any :non_form)
                              :map)
    #
    :conditional (sequence "#?"
                           (any :non_form)
                           :list)
    #
    :conditional_splicing (sequence "#?@"
                                    (any :non_form)
                                    :list)
    #
    :auto_resolve "::"
    #
    :var_quote (sequence "#'"
                         (any :non_form)
                         :form)
    #
    :eval (sequence "#="
                    (any :non_form)
                    (choice :list
                            :symbol))
    #
    :tag (sequence "#"
                   :symbol
                   (any :non_form)
                   (choice :tag
                           :collection
                           :literal))
    #
    :symbolic (sequence "##"
                        :symbol)
    #
    :backtick (sequence "`"
                        (any :non_form)
                        :form)
    #
    :quote (sequence "'"
                     (any :non_form)
                     :form)
    #
    :unquote (sequence "~"
                       (any :non_form)
                       :form)
    #
    :unquote_splicing (sequence "~@"
                                (any :non_form)
                                :form)
    #
    :deref (sequence "@"
                    (any :non_form)
                    :form)
    #
    :metadata
    (sequence (some (sequence (choice :metadata_entry
                                      :deprecated_metadata_entry)
                              (any :non_form)))
              (choice :collection
                      :conditional
                      :namespaced_map
                      :set
                      :tag
                      :fn
                      :unquote_splicing
                      :unquote
                      :deref
                      :quote
                      :backtick
                      :symbol))
    #
    :metadata_entry (sequence "^"
                              (choice :conditional
                                      :map
                                      :string
                                      :macro_keyword
                                      :keyword
                                      :symbol))
    #
    :deprecated_metadata_entry (sequence "#^"
                                         (choice :conditional
                                                 :map
                                                 :string
                                                 :macro_keyword
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
                     :macro_keyword
                     :keyword
                     :string
                     :character
                     :symbol)
    #
    :number (sequence (opt (set "+-"))
                      (some :digit)
                      (opt (choice :ratio_suffix
                                   :double_suffix
                                   :long_suffix)))
    #
    :digit (range "09")
    #
    :ratio_suffix (sequence "/"
                            (some :digit))
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
    :macro_keyword (sequence "::"
                             :keyword_head
                             (any :keyword_body))
    #
    :keyword (sequence ":"
                       (choice "/"
                               (sequence :keyword_head
                                         (any :keyword_body))))
    #
    :keyword_head (choice :allowed_name_character
                          (set "#'"))
    #
    :allowed_name_character
    (if-not (set "\f\n\r\t ()[]{}\"@~^;`\\,:#'/")
            1)
    #
    :keyword_body (choice (set ":/")
                          :keyword_head)
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
                         (choice :named_char
                                 :unicode
                                 :unicode_char))
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
    # XXX: this just matches anything...may be not what we want
    :unicode_char 1
    #
    :symbol (sequence :name_head
                      (any :name_body))
    #
    :name_head (choice "/"
                       :allowed_name_character)
    #
    :name_body (choice :name_head
                       (set ":#'"))
    })

(comment

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
