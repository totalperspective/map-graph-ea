(ns user ; user ns is loaded by REPL startup
  (:require [hyperfiddle.rcf]))

(hyperfiddle.rcf/enable!)

(comment
  (require '[nextjournal.clerk :as clerk])
    

  (clerk/serve! {:watch-paths ["notebooks" "src"]
               :browse true})
  )
