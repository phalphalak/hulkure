(ns hulkure.editor
  (:import [javax.swing JFrame JPanel JMenuBar JMenu JMenuItem KeyStroke AbstractAction]
           [java.awt Graphics Color]
           [java.awt.event KeyEvent ActionEvent ActionListener]))

(defn start-editor []
  (let [frame (JFrame. "Editor")
        actionListener (proxy [AbstractAction ActionListener] []
                         (actionPerformed [e]
                           (prn e)))]
    (.setDefaultCloseOperation frame JFrame/DISPOSE_ON_CLOSE)
    (.setContentPane frame (proxy [JPanel] []
                             (paintComponent [#^Graphics g]
                               (.setColor g Color/BLACK)
                               (.fillRect g 0 0 (.getWidth this) (.getHeight this)))))
    (.setJMenuBar frame (doto (JMenuBar.)
                    (.add (doto (JMenu. "File")
                            (.add (doto (JMenuItem. "New")
                                    (.setAccelerator (KeyStroke/getKeyStroke KeyEvent/VK_N ActionEvent/CTRL_MASK))
                                    (.addActionListener actionListener)))
                            (.addSeparator)
                            (.add (doto (JMenuItem. "Exit")
                                    (.setAccelerator (KeyStroke/getKeyStroke KeyEvent/VK_Q, ActionEvent/CTRL_MASK))
                                    (.addActionListener actionListener)))))))
    (.setSize frame 800 800)
    (.setVisible frame true)
   ))
