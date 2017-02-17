(ns tictactoe.core
  (:require [reagent.core :as reagent :refer [atom]]))

(enable-console-print!)

(defn new-board [n]
  (vec (repeat n (vec (repeat n "B")))))

(def board-size 3)

(def win-length 3)

(prn (new-board board-size))

(defonce app-state
  (atom {:text "Welcome to Tic-Tac-Toe!"
         :board (new-board board-size)
         :game-status :in-progress}))

(defn computer-move [board]
  (let [remaining-spots (for [i (range board-size)
                              j (range board-size)
                              :when (= (get-in board [j i]) "B")]
                          [j i])
        move (when (seq remaining-spots)
               (rand-nth remaining-spots))]
    (if move (assoc-in board move "C") board)))

(defn straight [owner board [x y] [dx dy] n]
  (every? true?
          (for [i (range 3)]
            (= (get-in board [(+ (* dx i) x)
                              (+ (* dy i) y)])
               owner))))

(defn win? [owner board n]
  (some true?
        (for [i (range board-size)
              j (range board-size)
              dir [[1 0] [0 1] [1 1] [1 -1]]]
          (straight owner board [i j] dir n))))

(defn full? [board]
  (every? #{"P" "C"} (apply concat board)))

(defn game-status [board]
  (cond
    (win? "P" board win-length) :player-victory
    (win? "C" board win-length) :computer-victory
    (full? board) :draw
    :else :in-progress))

(defn update-status
  [state]
  (assoc state :game-status (game-status (:board state))))

(defn check-game-status [state]
  (-> state
      (update-in [:board] computer-move)
      (update-status)))

(defn blank [i j]
  [:rect
   {:width 0.9
    :height 0.9
    :fill "grey"
    :x i
    :y j
    :on-click
    (fn blank-click [e]
      (when (= (:game-status @app-state) :in-progress)
        (swap! app-state assoc-in [:board j i] "P")
        (if (win? "P" (:board @app-state) win-length)
          (swap! app-state assoc :game-status :player-victory)
          (swap! app-state check-game-status))))}])

(defn circle [i j]
  [:circle
   {:r 0.40
    :fill "green"
    :cx (+ 0.5 i)
    :cy (+ 0.5 j)}])

(defn cross [i j]
  [:g {:stroke "darkred"
       :stroke-width 0.35
       :stroke-linecap "round"
       :transform
       (str "translate(" (+ 0.5 i) "," (+ 0.5 j) ")"
            "scale(0.35)")}
   [:line {:x1 -1 :y1 -1 :x2 1 :y2 1}]
   [:line {:x1 1 :y1 -1 :x2 -1 :y2 1}]])

(defn tictactoe []
  [:center
   [:h1 (:text @app-state)]
   [:h2
    (case (:game-status @app-state)
      :player-victory "You win!"
      :computer-victory "Computer wins."
      :draw "Draw"
      "")
    [:button
      {:on-click
       (fn new-game-click [e]
         (swap! app-state assoc
                :board (new-board board-size)
                :game-status :in-progress))}
      "New Game"]]
   (into
    [:svg
     {:view-box "0 0 3 3"
      :width 500
      :height 500}]
    (for [i (range (count (:board @app-state)))
          j (range (count (:board @app-state)))]
      (case (get-in @app-state [:board j i])
        "B" [blank i j]
        "P" [circle i j]
        "C" [cross i j])))])

(reagent/render-component [tictactoe]
                          (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  (prn (:board @app-state)))
