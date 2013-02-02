(ns hulkure.editor
  (:require [hulkure.board :as board])
  (:import [javax.swing JFrame JPanel JMenuBar JMenu JMenuItem KeyStroke AbstractAction]
           [java.awt Graphics Color]
           [java.awt.event KeyEvent ActionEvent ActionListener MouseAdapter MouseEvent]))

(defn reset-board! [state]
  (swap! state assoc :board (board/make-board)))

(defn paint [#^Graphics this state g]
  (.setColor g Color/BLACK)
  (.fillRect g 0 0 (.getWidth this) (.getHeight this))
  (.setColor g Color/GRAY)
  (dorun (map #(.fillRect g (inc (* 20 (% :x))) (inc (* 20 (% :y))) (- 20 2) (- 20 2))
              (get-in @state [:board :fields])))
  (when-let [mouse (@state :mouse-over)]
    (do
     (.setColor g Color/RED)
     (.drawRect g (* 20 (mouse :x)) (* 20 (mouse :y)) 20 20))))

(defn translate-screen-to-board [x y]
  {:x (int (/ x 20))
   :y (int (/ y 20))})

(defn make-mouse-adapter [state]
  (proxy [MouseAdapter] []
    (mouseMoved [#^MouseEvent event]
      (swap! state assoc :mouse-over (translate-screen-to-board(.getX event) (.getY event)))
      (.repaint (@state :panel)))
    (mouseClicked [#^MouseEvent event]
      (let [coords (translate-screen-to-board (.getX event) (.getY event))]
        (swap! state assoc :board (if (board/get-field (@state :board) coords)
                                    (board/remove-field (@state :board) coords)
                                    (board/set-field (@state :board) coords)))
        (.repaint (@state :panel))))))

(defn make-content-pane [state]
  (proxy [JPanel] []
    (paintComponent [#^Graphics g]
      (paint this state g))))

(defn make-action-new [state]
  (proxy [AbstractAction] ["New"]
    (actionPerformed [e]
      (reset-board! state)
      (.repaint (@state :panel)))))

(defn make-action-exit []
  (proxy [AbstractAction] ["Exit"]
    (actionPerformed [e]
      (System/exit 0))))

(defn start-editor [state]
  (let [frame (JFrame. "Editor")
        mouse-adapter (make-mouse-adapter state)
        content-pane (make-content-pane state)]
    (swap! state assoc :panel content-pane)
    (.setDefaultCloseOperation frame JFrame/DISPOSE_ON_CLOSE)
    (.setContentPane frame content-pane)
    (.setJMenuBar frame (doto (JMenuBar.)
                          (.add (doto (JMenu. "File")
                                  (.add (doto (JMenuItem. (make-action-new state))
                                          (.setAccelerator (KeyStroke/getKeyStroke KeyEvent/VK_N ActionEvent/CTRL_MASK))))
                                  (.addSeparator)
                                  (.add (doto (JMenuItem. (make-action-exit))
                                          (.setAccelerator (KeyStroke/getKeyStroke KeyEvent/VK_Q, ActionEvent/CTRL_MASK))))))))
    (.addMouseListener content-pane mouse-adapter)
    (.addMouseMotionListener content-pane mouse-adapter)
    (.addMouseWheelListener content-pane mouse-adapter)
    (.setSize frame 800 800)
    (.setVisible frame true)
   ))


(defn -main [& args]
  (let [state (atom {})]
    (reset-board! state)
    (start-editor state)))
