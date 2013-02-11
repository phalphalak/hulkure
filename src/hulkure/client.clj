(ns hulkure.client
  (:require [lamina.core :as lamina]
            [hulkure.io :as io]))

(defn connected-to-server [connection]
  (println "connected to server")
  (lamina/enqueue (connection :out-channel) {:action "register" :name "foo user"})
  (println (lamina/wait-for-message (connection :in-channel))))

(defn -main [& args]
  (io/create-client "localhost" 4000 connected-to-server))