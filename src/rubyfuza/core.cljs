(ns rubyfuza.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

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
  [["BR", "BK", "BB", "BG", "BQ", "BB", "BK", "BR"]
   ["BP", "BP", "BP", "BP", "BP", "BP", "BP", "BP"]
   [""  , ""  , ""  , ""  , ""  , ""  , ""  ,  ""]
   [""  , ""  , ""  , ""  , ""  , ""  , ""  ,  ""]
   [""  , ""  , ""  , ""  , ""  , ""  , ""  ,  ""]
   [""  , ""  , ""  , ""  , ""  , ""  , ""  ,  ""]
   ["WP", "WP", "WP", "WP", "WP", "WP", "WP", "WP"]
   ["WR", "WK", "WB", "WG", "WQ", "WB", "WK", "WR"]])

(def app-state (atom {:board starting-board}))

(defn unicode-pieces [board]
  (for [row board]
    (map #(get pieces % "") row)))

(defn draw-square [square]
  (dom/td nil (dom/a #js {:href "#"} square)))

(defn draw-row [row]
  (apply dom/tr nil
         (map draw-square row)))

(defn board [board owner]
  (om/component
   (apply dom/table #js {:className "chess_board"}
          (let [unicoded-board (unicode-pieces board)]
            (map draw-row unicoded-board)))))

(om/root
  app-state
  (fn [app owner]
    (om/build board (:board app)))
  (. js/document (getElementById "app")))
