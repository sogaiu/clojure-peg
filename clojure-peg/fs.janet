(import ./vendor/path)

(defn is-dir?
  [path]
  (when-let [path path
             stat (os/lstat path)]
    (= :directory (stat :mode))))

(comment

  (comment

    (is-dir? (os/getenv "HOME"))

    )

 )

(defn is-file?
  [path]
  (when-let [path path
             stat (os/lstat path)]
    (= :file (stat :mode))))

(comment

  (comment

    (is-file? (path/join (os/getenv "HOME") ".bashrc"))

    )

  (comment

    (is-file? (path/join (os/getenv "HOME") ".config/nvim/init.vim"))

    )

 )

(defn visit-files
  [path a-fn]
  (when (is-dir? path)
    (each thing (os/dir path)
      (def thing-path
        (path/join path thing))
      (cond
        (is-file? thing-path)
        (a-fn thing-path)
        #
        (is-dir? thing-path)
        (visit-files thing-path a-fn)))))

(comment

  (comment

    (visit-files (path/join (os/getenv "HOME")
                   "src/hpkgs/")
      |(eprint $))

    )

 (path/ext "./fs.janet")
 # => ".janet"

 (comment

   (import ./rewrite :fresh true)

   (let [start (os/time)]
     (visit-files (path/join (os/getenv "HOME")
                    #"src/clojars-cljish/clojure-interop")
                    #"src/clojars-cljish/org")
                    #"src/clojars-cljish/com")
                    #"src/clojars-cljish/io")
                    #"src/clojars-cljish/net")
                    #"src/clojars-cljish/binaryage")
                    #"src/clojars-cljish/aaron-santos")
                    "src/clojars-cljish")
       |(when (or (= (path/ext $) ".clj")
                (= (path/ext $) ".cljc")
                (= (path/ext $) ".cljs"))
          (let [src (slurp $)
                ast (try
                      (rewrite/ast src)
                      ([err]
                       (print "ast fail: " $)
                       nil))]
            (if (not ast)
              (eprint "ast nil: " $)
              (when (not= (string src)
                      (rewrite/code ast))
                (eprint $))))))
     (print (- (os/time) start) " secs"))

   )

 )
