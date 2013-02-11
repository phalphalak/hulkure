(ns hulkure.server
  (:require [hulkure.io :as io]
            [lamina.core :as lamina]
            [hulkure.game :as g]))

(defmulti process-client-request (fn [message game connection] (keyword (message :action))))

(defmethod process-client-request :register [message game connection]
  (comment (if-let [name (message :name)]
             (dosync

              (alter game #(assoc-in game [:participants (size (@game :participants))] )))
             ))
  (println game))

(defn client-connected [game connection]
  (println "client connected")
  (while true
    (process-client-request (lamina/wait-for-message (connection :in-channel)) game connection))
  ;(lamina/enqueue (connection :out-channel))
)

(defn -main [& args]
  (println "starting server")
  (let [game (ref (g/make-game))]
    (let [server (io/create-server #(client-connected game %) 4000)])))