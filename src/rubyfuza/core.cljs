(ns rubyfuza.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :as async :refer [put! chan <! >! timeout]]))

(enable-console-print!)

;;; Setup

(def pieces
  {"BR" "♜"
   "BK" "♞"
   "BB" "♝"
   "BQ" "♛"
   "BG" "♚"
   "BP" "♟"
   "WR" "♖"
   "WK" "♘"
   "WB" "♗"
   "WQ" "♕"
   "WG" "♔"
   "WP" "♙"})

(def starting-board
  [["BR", "BK", "BB", "BQ", "BG", "BB", "BK", "BR"] ;; A8 - H8
   ["BP", "BP", "BP", "BP", "BP", "BP", "BP", "BP"]
   [""  , ""  , ""  , ""  , ""  , ""  , ""  ,  ""]
   [""  , ""  , ""  , ""  , ""  , ""  , ""  ,  ""]
   [""  , ""  , ""  , ""  , ""  , ""  , ""  ,  ""]
   [""  , ""  , ""  , ""  , ""  , ""  , ""  ,  ""]
   ["WP", "WP", "WP", "WP", "WP", "WP", "WP", "WP"]
   ["WR", "WK", "WB", "WQ", "WG", "WB", "WK", "WR"]])

;;; Logic

(def col-map {"a" 0 "b" 1 "c" 2 "d" 3 "e" 4 "f" 5 "g" 6 "h" 7})

(defn coords-from [square]
  (let [column     (get col-map (first square))
        parsed-row (js/parseInt (last square))
        row        (- 8 parsed-row)]
    [row column]))

(defn move [board from-square to-square]
  "Returns an updated board with the move made"
  (let [from-coords (coords-from from-square)
        to-coords   (coords-from to-square)
        from-piece  (get-in board from-coords)]
    (-> board
        (assoc-in to-coords from-piece)
        (assoc-in from-coords ""))))

;;; Rendering

(defn draw-square [square]
  (dom/td nil (dom/a #js {:href "#"} square)))

(defn draw-row [row]
  (apply dom/tr nil
         (map draw-square row)))

(defn unicode-pieces [board]
  (for [row board]
    (map #(get pieces % "") row)))

(defn board [board owner]
  (om/component
   (apply dom/table #js {:className "chess_board"}
          (let [unicoded-board (unicode-pieces board)]
            (map draw-row unicoded-board)))))

;;; State

(def app-state (atom {:board starting-board}))
(def app-history (atom [@app-state]))

(defn start-chess! []
  (om/root
   app-state
   (fn [app owner]
     (om/build board (:board app)))
   (. js/document (getElementById "app"))))

(defn reset-and-start-chess! []
  (reset! app-state {:board starting-board})
  (reset! app-history [@app-state])
  (start-chess!))

(defn move! [from-square to-square]
  (swap! app-state
         update-in [:board] move from-square to-square)
  (swap! app-history
         conj @app-state))

;;; World interaction

(def the-world (chan))

(defn server-says [move-string]
  (let [from-square (subs move-string 0 2)
        to-square   (subs move-string 2)]
    (move! from-square to-square)))

(defn assimilate-novelty [world-channel]
  (go (while true
        (if-let [move-string (<! world-channel)]
          (server-says move-string)))))

(defn replay-game [world-channel moves]
    (dorun (map #(put! world-channel %) moves)))

;;; Playing with history

(defn show-move! [move-number]
  (reset! app-state (nth @app-history move-number)))

(defn slideshow [start finish]
  (go
   (doseq [step (range start finish)]
     (do
       (show-move! step)
       (<! (timeout 150))))))

(defn cycle-every-state []
  ;; Show that react.js only renders when it needs to
   (doseq [step (range 0 (count @app-history))]
     (show-move! step)))

;;; Main

; Start listening to changes in the world
(assimilate-novelty the-world)

;;; Demo

; Garry Kasparov - Veselin Topalov
; Hoogovens A Tournament Wijk aan Zee NED 1999.01.20
(defn kasparov-topalov [the-world]
  (let [moves ["e2e4" "d7d6" "d2d4" "g8f6" "b1c3" "g7g6" "c1e3" "f8g7" "d1d2" "c7c6" "f2f3" "b7b5" "g1e2" "b8d7" "e3h6" "g7h6" "d2h6" "c8b7" "a2a3" "e7e5" "e1c1" "d8e7" "c1b1" "a7a6" "e2c1" "e8c8" "c1b3" "e5d4" "d1d4" "c6c5" "d4d1" "d7b6" "g2g3" "c8b8" "b3a5" "b7a8" "f1h3" "d6d5" "h6f4" "b8a7" "h1e1" "d5d4" "c3d5" "b6d5" "e4d5" "e7d6" "d1d4" "c5d4" "e1e7" "a7b6" "f4d4" "b6a5" "b2b4" "a5a4" "d4c3" "d6d5" "e7a7" "a8b7" "a7b7" "d5c4" "c3f6" "a4a3" "f6a6" "a3b4" "c2c3" "b4c3" "a6a1" "c3d2" "a1b2" "d2d1" "h3f1" "d8d2" "b7d7" "d2d7" "f1c4" "b5c4" "b2h8" "d7d3" "h8a8" "c4c3" "a8a4" "d1e1" "f3f4" "f7f5" "b1c1" "d3d2" "a4a7"]]
    (replay-game the-world moves)))

(defn run-demo []
  (reset-and-start-chess!)
  (kasparov-topalov the-world)
  (go
   (slideshow 0 (count @app-history))))

(set! (.-onload js/window) run-demo)

;; The staments below can all be evaluated in Light Table

; Copied and pasted from https://github.com/swannodette/om/wiki/Tutorial :
; Type the key chord Control-SPACE to open up the command list. Start typing Add Connection, press enter to select it. In the list of options select Browser (External). Copy and paste the script tag into index.html before the <div id="app"></div>.

(comment
  ; Run the demo
  (run-demo)

  ; Show the starting board
  starting-board

  ; Show the board after a move has been made
  (move starting-board "a8" "b8")

  ; Reset the app state and draw the board
  (reset-and-start-chess!)

  ; Send a message to the world channel that will ultimately cause a move
  (put! the-world "e2e4")

  ; "Play" a famous game and the state will be the final move
  (reset-and-start-chess!)
  (kasparov-topalov the-world)

  ; Show how many moves have been played
  (count @app-history)

  ; Play the first 20 moves through
  (slideshow 0 20)
  ; Play the whole game through
  (slideshow 0 (count @app-history))

  ;; Muck about with history
  (show-move! 17)
  ; Show that by cycling every state does not cause a redraw on every state change
  (cycle-every-state)
  (show-move! 77)

  ; Show the current app-state
  @app-state
)
