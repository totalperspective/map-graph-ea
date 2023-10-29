(ns user ; user ns is loaded by REPL startup
  (:require [hyperfiddle.rcf]
            [nextjournal.clerk :as clerk]
            [portal.api :as p]))

(hyperfiddle.rcf/enable!)

(clerk/serve! {:watch-paths ["notebooks" "src"]})

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def p (p/open {:launcher :vs-code}))

(add-tap #'p/submit) ; Add portal as a tap> target

(tap> :hello) ; Start tapping out values
