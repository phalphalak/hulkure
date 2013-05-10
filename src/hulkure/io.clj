(ns hulkure.io
  (:require [lamina.core :as lamina]
            [clojure.data.json :as json])
  (:use [clojure.tools.logging :only (debug info error)])
  (:import [java.net ServerSocket Socket SocketException]
           [java.io PrintWriter InputStreamReader BufferedReader]))

(defn- on-thread
  ([f] (on-thread f nil))
  ([f name]
      (let [thread (Thread. #^Runnable f)]
        (when name (.setName thread name))
        (.start thread))))

(defn- close-socket [#^Socket socket]
  (debug "close socket:" socket)
  (when-not (.isClosed socket)
    (doto socket
      (.shutdownInput)
      (.shutdownOutput)
      (.close))))

(defn resolve-recipients [server recipient-ids]
  (if (= recipient-ids :all)
    @(server :connections)
    (filter #((set (if (vector? recipient-ids)
                     recipient-ids
                     [recipient-ids])) (% :id))
            @(server :connections))))

(defn broadcast [message server]
  (dorun (pmap #(do (debug (str "sending to socket (id = " (% :id) ")"))
                    (.println (% :out-socket) message))
               (resolve-recipients server (message :recipient)))))

(defn server-status [server]
  (str "current server status: "
       "connections: " (count @(server :connections))))

(defn close-connection [connection & [server]]
  (info (str "close connection " connection))
  (when-not @(connection :exit)
    (reset! (connection :exit) true)
    (let [{:keys [socket in-channel out-channel]} connection]
      (lamina/close in-channel)
      (close-socket socket)
      (when server
        (dosync (commute (server :connections) disj connection))
        (lamina/enqueue (server :out-channel) {:recipient :all
                         :message (str "Client disconnected (id = " (connection :id) ")")}))))
  (when server
    (debug (server-status server))))

(defn read-json [string]
  (try (json/read-str string :key-fn keyword)
       (catch Exception e
         {:error (str "Received invalid json: " string)})))

(defn read-json-map [string]
  (let [data (read-json string)]
    (if (instance? clojure.lang.PersistentArrayMap data)
      data
      {:error (str "Received json is not a map: " string)})))

(defn listen-on-socket [connection server]
  (try
    (while (not @(connection :exit))
      (debug (str "listening on socket"))
      (let [message (.readLine (connection :in-socket))]
        (if message
          (let [json-msg (assoc (read-json-map message) :client-id (connection :id))]
            (do (debug (str "received from socket: " json-msg))
                (lamina/enqueue (server :in-channel) json-msg)))
          (close-connection connection server))))
    (catch Exception e
      (error (.getMessage e))
      (close-connection connection server))))

(defn listen-on-channel [server]
  (let [channel (server :out-channel)]
    (while true
      (debug (str "waiting on out-channel"))
      (let [message (lamina/wait-for-message channel)]
        ;; TODO sanitize (remove recipient etc)
        (debug (str "received from out-channel: " message))
        (broadcast message server)))))

(defn new-connection [connection & [server]]
  (on-thread #(listen-on-socket connection server) (str (:id connection) "-socket")))

(defn accept [#^Socket socket server]
  (debug "new client connection accepted")
  (dosync (let [connection {:exit (atom false)
                            :socket socket
                            :in-socket (BufferedReader. (InputStreamReader. (.getInputStream socket)))
                            :out-socket (PrintWriter. (.getOutputStream socket) true)
                            :in-channel (lamina/fork (server :in-channel))
                            :id (inc (apply max (cons -1 (map :id @(server :connections)))))}]
            (alter (server :connections) conj connection)
            (new-connection connection server))))

(defn create-client [host port fun]
  (let [socket (Socket. host port)
        connection {:socket socket
                    :in-socket (BufferedReader. (InputStreamReader. (.getInputStream socket)))
                    :out-socket (PrintWriter. (.getOutputStream socket) true)
                    :in-channel (lamina/map* json/write-str (lamina/channel))
                    :exit (atom false)}]
    (try
      (new-connection connection fun)
      (catch SocketException e
        (println (.getMessage e))))
    (println "connection done")
    (close-connection connection)))

(defn close-server [server]
  (doseq [connection @(server :connections)]
    (close-connection connection server))
  (.close #^ServerSocket (server :server-socket)))

(defn create-server [port]
  (let [server-socket (ServerSocket. port)
        connections (ref #{})
        server {:server-socket server-socket
                :connections connections
                :in-channel (lamina/channel)
                :out-channel (lamina/channel)}]
    (on-thread #(listen-on-channel server) "channel-out")
    (on-thread #(do (while (not (.isClosed server-socket))
                      (debug "listening on server socket")
                      (try
                        (accept (.accept server-socket) server)
                        (catch SocketException e
                          (println (.message e)))))
                    (close-server server))
               "server")
    server))
