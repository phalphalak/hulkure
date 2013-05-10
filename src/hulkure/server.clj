(ns hulkure.server
  (:require [hulkure.io :as io]
            [lamina.core :as lamina]
            [hulkure.game :as g])
  (:use [clojure.tools.logging :only (debug info error)]))

(defmulti process-client-request (fn [message game connection] (if (message :error) :error (keyword (message :action)))))

(defmethod process-client-request :error [message game out-channel]
  (debug "handling error: " message)
  (lamina/enqueue out-channel (assoc message :recipient (message :client-id))))

(defmethod process-client-request :register [message game out-channel]
  (if-let [name (message :name)]
    (lamina/enqueue out-channel (dosync
                                 (if (some #{name} (map :name (@game :participants)))
                                   {:error (str "name " name " already taken")
                                    :recipient (message :client-id)}
                                   (do (alter game assoc :participants (vec (cons {:id (message :client-id)
                                                                                   :name name} (@game :participants))))
                                       {:info (str "Player " name " joined")
                                        :recipient :all}))))
    (lamina/enqueue out-channel {:error "expected name" :original-message message})))

(defmethod process-client-request :default [message game out-channel]
  (lamina/enqueue out-channel (merge {:error (if-let [action (message :action)]
                                               (str "unknown action '" action "'")
                                               "missing action")
                                      :recipient (message :client-id)} message)))

(defn server-loop [game in-channel out-channel]
  (while true
    (debug "waiting on in-channel")
    ; TODO sanitize messages (remove client-id, error, recipient for instance)
    (process-client-request (lamina/wait-for-message in-channel) game out-channel)))

(defn -main [& args]
  (info "starting server")
  (let [game (ref (g/make-game))
        server (io/create-server 4000)]
    (server-loop game
                 (server :in-channel)
                 (server :out-channel))))