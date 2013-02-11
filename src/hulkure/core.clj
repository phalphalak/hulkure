(ns hulkure.core
  (:require [hulkure.editor :as editor]
            [hulkure.server :as server]
            [hulkure.client :as client])
  (:gen-class :main true))

(set! *warn-on-reflection* true)

(defn -main [what & args]
  (condp = what
    "server" (server/-main args)
    "client" (client/-main args)
    "editor" (editor/-main args)))
