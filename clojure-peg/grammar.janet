(def cg
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
    :keyword (sequence ":"
                       (choice "/"
                               (sequence :keyword_head
                                         (any :keyword_body))))
    #
    :keyword_head (choice :allowed_name_character
                          (set "#'"))
    #
    :allowed_name_character
    (if-not (set "\r\n\t\f ()[]{}\"@~^;`\\,:#'/")
            1)
    #
    :keyword_body (choice (set ":/")
                          :keyword_head)
    #
    :macro_keyword (sequence "::"
                             :keyword_head
                             (any :keyword_body))
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
    :number (sequence (opt (set "+-"))
                      (some :digit)
                      (choice :ratio_suffix
                              :double_suffix
                              :long_suffix))
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
    :character (sequence "\\"
                         (choice :named_char
                                 :unicode
                                 :unicode_char))
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
    :symbol (sequence :name_head
                      (any :name_body))
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
                      :symbol))
    #
    :metadata_entry (sequence "^"
                              (choice :map
                                      :string
                                      :macro_keyword
                                      :keyword
                                      :symbol))
    #
    :deprecated_metadata_entry (sequence "#^"
                                         (choice :map
                                                 :string
                                                 :macro_keyword
                                                 :keyword
                                                 :symbol))
    #
    :backtick (sequence "`"
                        (opt :whitespace)
                        :form)
    #
    :quote (sequence "'"
                     (opt :whitespace)
                     :form)
    #
    :unquote (sequence "~"
                       (opt :whitespace)
                       :form)
    #
    :unquote_splicing (sequence "~@"
                                (opt :whitespace)
                                :form)
    #
    :deref (sequence "@"
                    (opt :whitespace)
                    :form)
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
    :fn (sequence "#" :list)
    #
    :regex (sequence "#" :string)
    #
    :set (sequence "#{"
                   (any :input)
                   (choice "}" (error "")))
    #
    :namespaced_map (sequence "#"
                              (choice :macro_keyword
                                      :auto_resolve
                                      :keyword)
                              (opt :whitespace)
                              :map)
    #
    :auto_resolve "::"
    #
    :var_quote (sequence "#'"
                         (opt :whitespace)
                         :form)
    #
    :discard (sequence "#_"
                       (opt (sequence (opt :whitespace)
                                      :discard))
                       (opt :whitespace)
                       :form)
    #
    :tag (sequence "#"
                   :symbol
                   (opt :whitespace)
                   (choice :tag
                           :collection
                           :literal))
    #
    :conditional (sequence "#?"
                           (opt :whitespace)
                           :list)
    #
    :conditional_splicing (sequence "#?@"
                                    (opt :whitespace)
                                    :list)
    #
    :symbolic (sequence "##"
                        :symbol)
    #
    :eval (sequence "#="
                    (opt :whitespace)
                    (choice :list
                            :symbol))
    #
    :whitespace (some (set "\f\n\r\t, "))
    #
    :comment (sequence (choice ";"
                               "#!")
                       (any (if-not (set "\r\n")
                                    1)))
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
