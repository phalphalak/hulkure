(ns hulkure.io
  (:require [lamina.core :as lamina]
            [clojure.data.json :as json])
  (:import [java.net ServerSocket Socket SocketException]
           [java.io PrintWriter InputStreamReader BufferedReader]))


(defn- on-thread [f]
  (doto (Thread. #^Runnable f)
    (.start)))
(defn- close-socket [#^Socket socket]
  (when-not (.isClosed socket)
    (doto socket
      (.shutdownInput)
      (.shutdownOutput)
      (.close))))

(defn close-connection [connection & [server]]
  (println (str "close connection " connection))
  (when-not @(connection :exit)
    (reset! (connection :exit) true)
    (let [{:keys [socket in-channel out-channel]} connection]
      (lamina/close in-channel)
      (lamina/close out-channel)
      (close-socket socket)
      (when server
        (dosync (commute (server :connections) disj connection)))))
  (when server
    (println (str "current server status: " server))))

(defn read-json [string]
  (try (json/read-str string :key-fn keyword)
       (catch Exception e
         {:error (str "Received invalid json: " string)})))

(defn listen-on-socket [connection server]
  (try
    (while (not @(connection :exit))
      (let [message (.readLine (connection :in-socket))]
        (when message
          (let [json-msg (read-json message)]
            (do (println (str "received from socket: " json-msg))
                (lamina/enqueue (connection :in-channel) json-msg))))))
    (catch SocketException e
      (when-not @(connection :exit)
        (println (.getMessage e))
        (close-connection connection server)))))

(defn listen-on-channel [connection server]
  (try
    (while (not @(connection :exit))
      (let [message (lamina/wait-for-message (lamina/map* json/write-str (connection :out-channel)))]
        (println (str "received from channel: " message))
        (.println (connection :out-socket) message)))
    (catch IllegalStateException e
      (when-not @(connection :exit)
        (println (.getMessage e))
        (close-connection connection server)))))

(defn new-connection [connection fun & [server]]
  (on-thread #(listen-on-socket connection server))
  (on-thread #(listen-on-channel connection server))
  (fun connection))


(defn accept [#^Socket socket server fun]
  (let [connection {:exit (atom false)
                    :socket socket
                    :in-socket (BufferedReader. (InputStreamReader. (.getInputStream socket)))
                    :out-socket (PrintWriter. (.getOutputStream socket) true)
                    :in-channel (lamina/channel)
                    :out-channel (lamina/channel)}]
    (on-thread #(do
                  (dosync (commute (server :connections) conj connection))
                  (try
                    (new-connection connection fun server)
                    (catch SocketException e
                      (println (.getMessage e))))
                  (println "accepted connection done")
                  (close-connection connection server)))))

(defn create-client [host port fun]
  (let [socket (Socket. host port)
        connection {:socket socket
                    :in-socket (BufferedReader. (InputStreamReader. (.getInputStream socket)))
                    :out-socket (PrintWriter. (.getOutputStream socket) true)
                    :in-channel (lamina/channel)
                    :out-channel (lamina/channel)
                    :exit (atom false)}]
    (try
      (new-connection connection fun)
      (catch SocketException e
        (println (.getMessage e))))
    (println "connection done")
    (close-connection connection)))

(defn create-server [fun port]
  (let [server-socket (ServerSocket. port)
        connections (ref #{})
        server {:server-socket server-socket
                :connections connections}]
    (on-thread #(when-not (.isClosed server-socket)
                  (try
                    (accept (.accept server-socket) server fun)
                    (catch SocketException e
                      (println (.message e))))
                  (recur)))
    server))

(defn close-server [server]
  (doseq [connection @(server :connections)]
    (close-connection connection server))
  (.close #^ServerSocket (server :server-socket)))
