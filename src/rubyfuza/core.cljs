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
  [["BR", "BK", "BB", "BG", "BQ", "BB", "BK", "BR"] ;; A8 - H8
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

(def col-map
  {"a" 0
   "b" 1
   "c" 2
   "d" 3
   "e" 4
   "f" 5
   "g" 6
   "h" 7})

(defn index-from [position]
  (let [row (get col-map (first position))
        parsed-column (js/parseInt (last position))
        column (- 8 parsed-column)]
    [column row]))

(defn value-at [board position]
  (let [[row column](index-from position)
        the-row (nth board row)]
    (nth the-row row)))

(defn move [board from to]
  (let [from-piece (value-at board from)
        from-coords (index-from from)
        to-coords (index-from to)]
    (-> board
        (assoc-in to-coords from-piece)
        (assoc-in from-coords ""))))

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
