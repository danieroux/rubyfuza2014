(ns rubyfuza.core
  (:require-macros [cljs.core.async.macros :refer [go alt!]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :as async :refer [put! chan <! >!]]))

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

(def col-map
  {"a" 0
   "b" 1
   "c" 2
   "d" 3
   "e" 4
   "f" 5
   "g" 6
   "h" 7})

(defn coords-from [square]
  (let [column (get col-map (first square))
        parsed-row (js/parseInt (last square))
        row (- 8 parsed-row)]
    [row column]))

(defn move [board from-square to-square]
  (let [from-coords (coords-from from-square)
        to-coords (coords-from to-square)
        from-piece (get-in board from-coords)]
    (-> board
        (assoc-in to-coords from-piece)
        (assoc-in from-coords ""))))

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

(om/root
  app-state
  (fn [app owner]
    (om/build board (:board app)))
  (. js/document (getElementById "app")))

(defn move! [from-square to-square]
  (swap! app-state update-in [:board] move from-square to-square))

(def app-history
  (atom [@app-state]))

(add-watch app-state :history
  (fn [_ _ _ n]
    (when-not (= (last @app-history) n)
      (swap! app-history conj n))))

(defn undo-move []
  (when (> (count @app-history) 1)
    (swap! app-history pop)
    (reset! app-state (last @app-history))))

(def the-world (chan))

(defn assimilate-novelty [world-channel]
  (go (while true
        (if-let [[from-square to-square] (<! world-channel)]
          (move! from-square to-square)))))

(defn fake-server-move [world-channel a-move]
  (put! world-channel a-move))

(assimilate-novelty the-world)

;(fake-server-move the-world ["a8" "c8"])
