(ns hulkure.editor
  (:require [hulkure.board :as board]
            [clojure.data.json :as json])
  (:import [javax.swing JFrame JPanel JMenuBar JMenu JMenuItem KeyStroke AbstractAction JFileChooser JOptionPane]
           [java.awt Graphics Color]
           [java.io File]
           [java.awt.event KeyEvent ActionEvent ActionListener MouseAdapter MouseEvent]))

(defn reset-board! [state]
  (swap! state assoc :board (board/make-board))
  (swap! state assoc :current-tile {:id 0 :data {:type :corridor}})
  (swap! state assoc :save-file nil))

(defn paint [#^Graphics this state g]
  (.setColor g Color/BLACK)
  (.fillRect g 0 0 (.getWidth this) (.getHeight this))
  (.setColor g Color/GRAY)
  (dorun (map #(.fillRect g (inc (* 20 (% :x))) (inc (* 20 (% :y))) (- 20 2) (- 20 2))
              (get-in @state [:board :fields])))
  (when-let [mouse (@state :mouse-over)]
    (do
     (.setColor g Color/RED)
     (.drawRect g (* 20 (mouse :x)) (* 20 (mouse :y)) 20 20)
     (.setColor g Color/WHITE)
     (.drawString g (str "x: " (mouse :x) ", y: " (mouse :y)) 5 15)))
  (.setColor g Color/WHITE)
  (.drawString g (str "current tile: " (@state :current-tile)) 5 30))

(defn translate-screen-to-board [x y]
  {:x (int (/ x 20))
   :y (int (/ y 20))})

(defn make-mouse-adapter [state]
  (proxy [MouseAdapter] []
    (mouseMoved [#^MouseEvent event]
      (swap! state assoc :mouse-over (translate-screen-to-board(.getX event) (.getY event)))
      (.repaint (@state :panel)))
    (mouseClicked [#^MouseEvent event]
      (let [coords (translate-screen-to-board (.getX event) (.getY event))
            field (merge coords {:tile-id (get-in @state [:current-tile :id])})]
        (swap! state assoc :board (if (board/get-field (@state :board) coords)
                                    (board/remove-field (@state :board) coords)
                                    (board/set-tile (board/set-field (@state :board) field)
                                                    (get-in @state [:current-tile :id])
                                                    (get-in @state [:current-tile :data]))))
        (prn @state)
        (.repaint (@state :panel))))))

(defn make-panel [state]
  (proxy [JPanel] []
    (paintComponent [#^Graphics g]
      (paint this state g))))

(defn make-action-new [state]
  (proxy [AbstractAction] ["New"]
    (actionPerformed [e]
      (reset-board! state)
      (.repaint (@state :panel)))))

(defn- load-tile! [state id]
  (let [tile (or (board/get-tile (@state :board) id)
                 {:type :corridor})]
    (swap! state assoc :current-tile {:id id :data tile})))

(defn- make-action-change-current-tile-id [state change]
  (proxy [AbstractAction] []
    (actionPerformed [e]
      (load-tile! state (change (get-in @state [:current-tile :id])))
      (.repaint (@state :panel)))))

(defn make-action-increment-tile-id [state]
  (make-action-change-current-tile-id state inc))

(defn make-action-decrement-tile-id [state]
  (make-action-change-current-tile-id state dec))

(defn make-action-toggle-room-tile [state]
  (proxy [AbstractAction] []
    (actionPerformed [e]
      (prn @state)
      (let [new-type (if (= :room (get-in @state [:current-tile :data :type]))
                       :corridor
                       :room)]
        (swap! state assoc-in [:current-tile :data :type] new-type)
        (when (board/get-tile (@state :board) (get-in @state [:current-tile :id]))
          (swap! state assoc :board (board/set-tile (@state :board)
                                                    (get-in @state [:current-tile :id])
                                                    (get-in @state [:current-tile :data]))))))))

;TODO let save and open dialog use same file-chooser
(defn make-action-load [state]
  (let [file-chooser (JFileChooser. (File. "."))]
    (proxy [AbstractAction] ["Load"]
      (actionPerformed [e]
        (when (= JFileChooser/APPROVE_OPTION (.showOpenDialog file-chooser (@state :frame)))
          (let [file (.getSelectedFile file-chooser)]
            (reset-board! state)
            (swap! state assoc :save-file file)
            (swap! state assoc :board (board/load-board (.getAbsoluteFile file)))
            (load-tile! state 0)
            (.repaint (@state :panel))))))))

(defn make-action-save-as [state]
  (let [file-chooser (JFileChooser. (File. "."))]
    (proxy [AbstractAction] ["Save as"]
      (actionPerformed [e]
        (when (= JFileChooser/APPROVE_OPTION (.showSaveDialog file-chooser (@state :frame)))
          (let [file (.getSelectedFile file-chooser)]
            (when (or (not (.exists file))
                      (and (.isFile file)
                           (= JOptionPane/YES_OPTION
                              (JOptionPane/showConfirmDialog (@state :frame)
                                                             (str "File " (.getName file) " already exists. Do you wish to overwrite it?")
                                                             "File already exists"
                                                             JOptionPane/YES_NO_OPTION))))
              (swap! state assoc :save-file file)
              (spit (.getAbsoluteFile file) (json/write-str (@state :board))))))))))

(defn make-action-exit []
  (proxy [AbstractAction] ["Exit"]
    (actionPerformed [e]
      (System/exit 0))))

(defn start-editor [state]
  (let [frame (JFrame. "Editor")
        mouse-adapter (make-mouse-adapter state)
        panel (make-panel state)
        content-pane (.getContentPane frame)]
    (swap! state assoc :frame frame)
    (swap! state assoc :panel panel)
    (doto panel
      (.addMouseListener mouse-adapter)
      (.addMouseMotionListener mouse-adapter)
      (.addMouseWheelListener mouse-adapter))
    (.put (.getInputMap panel)
          (KeyStroke/getKeyStroke KeyEvent/VK_ADD 0)
          "inc tile id")
    (.put (.getInputMap panel)
          (KeyStroke/getKeyStroke KeyEvent/VK_SUBTRACT 0)
          "dec tile id")
    (.put (.getInputMap panel)
          (KeyStroke/getKeyStroke KeyEvent/VK_R 0)
          "toggle room tile")
    (.put (.getActionMap panel)
          "inc tile id"
          (make-action-increment-tile-id state))
    (.put (.getActionMap panel)
          "dec tile id"
          (make-action-decrement-tile-id state))
    (.put (.getActionMap panel)
          "toggle room tile"
          (make-action-toggle-room-tile state))
    (doto frame
      (.setContentPane panel)
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (.setJMenuBar (doto (JMenuBar.)
                      (.add (doto (JMenu. "File")
                              (.add (doto (JMenuItem. (make-action-new state))
                                      (.setAccelerator (KeyStroke/getKeyStroke KeyEvent/VK_N ActionEvent/CTRL_MASK))))
                              (.add (doto (JMenuItem. (make-action-load state))
                                      (.setAccelerator (KeyStroke/getKeyStroke KeyEvent/VK_L ActionEvent/CTRL_MASK))))
                              (.add (doto (JMenuItem. (make-action-save-as state))))
                              (.addSeparator)
                              (.add (doto (JMenuItem. (make-action-exit))
                                      (.setAccelerator (KeyStroke/getKeyStroke KeyEvent/VK_Q, ActionEvent/CTRL_MASK))))))))
      (.setSize 800 800)
      (.setVisible true))))


(defn -main [& args]
  (let [state (atom {})]
    (reset-board! state)
    (start-editor state)))
