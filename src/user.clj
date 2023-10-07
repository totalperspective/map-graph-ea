(ns user ; user ns is loaded by REPL startup
  (:require [hyperfiddle.rcf]
            [nextjournal.clerk :as clerk]))

(hyperfiddle.rcf/enable!)

(clerk/serve! {:watch-paths ["notebooks" "src"]})
