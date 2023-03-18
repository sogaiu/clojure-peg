(def sep
  (if (= :windows (os/which))
    "\\"
    "/"))

(defn path-join
  [left right &opt sepa]
  (default sepa sep)
  (string left sepa right))

(comment

  (path-join "/tmp" "test.txt" "/")
  # =>
  "/tmp/test.txt"

  (path-join "C:\\windows" "system32" "\\")
  # =>
  "C:\\windows\\system32"

  )

(defn path-ext
  [path]
  (let [results (string/find-all "." path)]
    (if-let [last-one (last results)]
      (string/slice path last-one)
      "")))

(comment

  (path-ext "hello.janet")
  # =>
  ".janet"

  (path-ext "bye")
  # =>
  ""

  )

(defn is-dir?
  [path]
  (when-let [path path
             stat (os/lstat path)]
    (= :directory (stat :mode))))

(comment

  (is-dir? (or (os/getenv "HOME")
               (os/getenv "USERPROFILE")))
  # =>
  true

 )

(defn is-file?
  ``
  Returns true if `path` is an ordinary file (e.g. not a directory).
  Otherwise, returns false.
  ``
  [path]
  (truthy?
    (when-let [path path
               mode-stat (os/lstat path :mode)]
      (= :file mode-stat))))

(comment

  (is-file? (or (os/getenv "HOME")
                (os/getenv "USERPROFILE")))
  # =>
  false

  (let [name (string (gensym))]
    (if (os/stat name)
      true
      (do
        (spit name "hello")
        (def res
          (is-file? name))
        (os/rm name)
        res)))
  # =>
  true

 )

(defn just-files
  ``
  Recursively visit directory tree starting at `path`, accumulating
  file (not directory) paths by default into array `acc`.  If optional
  argument `a-fn` is specified, instead accumulate only file paths
  for which `a-fn` applied to the file path returns a truthy result.
  ``
  [path acc &opt a-fn]
  (default a-fn identity)
  (when (is-dir? path)
    (each thing (os/dir path)
      (def thing-path
        (path-join path thing))
      (cond
        (and (is-file? thing-path)
             (a-fn thing-path))
        (array/push acc thing-path)
        #
        (is-dir? thing-path)
        (just-files thing-path acc a-fn))))
  acc)

(comment

  (def acc @[])

  (just-files (string (os/getenv "HOME")
                      "/.config")
              acc)

  )

(defn just-dirs
  ``
  Recursively visit directory tree starting at `path`, accumulating
  directory paths by default into array `acc`.  If optional
  argument `a-fn` is specified, instead accumulate only directory paths
  for which `a-fn` applied to the directory path returns a truthy result.
  ``
  [path acc &opt a-fn]
  (default a-fn identity)
  (when (is-dir? path)
    (each thing (os/dir path)
      (def thing-path
        (path-join path thing))
      (when (is-dir? thing-path)
        (when (a-fn thing-path)
          (array/push acc thing-path))
        (just-dirs thing-path acc a-fn))))
  acc)

(comment

  (def acc @[])

  (just-dirs (path-join (os/getenv "HOME")
                        "/.config")
             acc)

 )

(defn visit-files
  ``
  Recursively visit directory tree starting at `path`.  If optional
  argument `a-fn` is specified, apply `a-fn` to each encountered file
  (not directory) path.
  ``
  [path a-fn]
  (when (is-dir? path)
    (each thing (os/dir path)
      (def thing-path
        (path-join path thing))
      (cond
        (is-file? thing-path)
        (a-fn thing-path)
        #
        (is-dir? thing-path)
        (visit-files thing-path a-fn)))))

(comment

  (visit-files (path-join (os/getenv "HOME")
                          ".config")
               |(eprint $))

 )

(defn visit-dirs
  ``
  Recursively visit directory tree starting at `path`.  If optional
  argument `a-fn` is specified, apply `a-fn` to each encountered directory
  path.
  ``
  [path a-fn]
  (when (is-dir? path)
    (each thing (os/dir path)
      (def thing-path
        (path-join path thing))
      (when (is-dir? thing-path)
        (a-fn thing-path)
        (visit-dirs thing-path a-fn)))))

(comment

  (visit-dirs (path-join (os/getenv "HOME")
                         ".config")
              |(eprint $))

 )

(defn visit
  ``
  Recursively visit directory tree starting at `path`.  If optional
  argument `a-fn` is specified, apply `a-fn` to each encountered
  path (file and directory).
  ``
  [path a-fn]
  (when (is-dir? path)
    (each thing (os/dir path)
      (def thing-path
        (path-join path thing))
      (when (or (is-file? thing-path)
                (is-dir? thing-path))
        (a-fn thing-path))
      (when (is-dir? thing-path)
        (visit thing-path a-fn)))))

(comment

  (visit (path-join (os/getenv "HOME")
                    ".config")
         |(eprint $))

 )

(comment

 (comment

   (import ./rewrite :fresh true)

   # fails on clojars-cljish/shen/clj/shen.clj/0.1.6/shen.clj-0.1.6.jar/shen.clj
   (let [start (os/time)]
     (visit-files (path-join (os/getenv "HOME")
                             "src/clojars-cljish")
                  |(when (or (= (path-ext $) ".clj")
                             (= (path-ext $) ".cljc")
                             (= (path-ext $) ".cljs"))
                     (let [src (slurp $)
                           ast (try
                                 (rewrite/par src)
                                 ([err]
                                   (print "ast fail: " $)
                                   nil))]
                       (if (not ast)
                         (eprint "ast nil: " $)
                         (when (not= (string src)
                                     (rewrite/gen ast))
                           (eprint $))))))
     (print (- (os/time) start) " secs"))

   )

 )
