(import ./clojure-peg/vendor/path)

(declare-project
 :name "clojure-peg"
 :url "https://github.com/sogaiu/clojure-peg"
 :repo "git+https://github.com/sogaiu/clojure-peg.git")

(def proj-root
  (os/cwd))

(def proj-dir-name
  "clojure-peg")

(def src-root
  (path/join proj-root proj-dir-name))

(declare-source
 :source [src-root])

(phony "netrepl" []
       (os/execute
        ["janet" "-e" (string "(os/cd \"" src-root "\")"
                              "(import spork/netrepl)"
                              "(netrepl/server)")] :p))

