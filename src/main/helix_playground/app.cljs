(ns helix-playground.app
  (:require [helix.core  :refer [defnc $]]
            [helix.hooks :as    hooks]
            [helix.dom   :as    d]
            ["react-dom" :as    rdom]))

;; Following along with https://reactjs.org/tutorial/tutorial.html

(defn- current-player
  [step]
  (if (even? step) "X" "O"))

(defn- determine-winner
  "Returns \"X\", \"O\", or nil depending on who won, if anyone."
  [squares]
  (first
    (for [line [[0 1 2] [3 4 5] [6 7 8]
                [0 3 6] [1 4 7] [2 5 8]
                [0 4 8] [2 4 6]]
          :let [[a b c] (map #(get squares %) line)]
          :when (and a (= a b c))]
      a)))

(defn- tie?
  [squares]
  (and (not (determine-winner squares))
       (not-any? nil? squares)))

(defnc square
  [{:keys [value on-click]}]
  (d/button
    {:class    "square"
     :on-click on-click}
    value))

(defnc board
  [{:keys [squares on-click]}]
  (let [render-square
        (fn [i]
          ($ square {:value    (get squares i)
                     :on-click #(on-click i)}))]
    (d/div
      (for [row-indices (partition 3 (range 9))]
        (d/div
          {:class "board-row"}
          (for [i row-indices]
            (render-square i)))))))

(defnc game
  [_]
  (let [[{:keys [history current-step]} set-state]
        (hooks/use-state
          {:history      [{:squares (vec (repeat 9 nil))}]
           :current-step 0})

        {:keys [squares]}
        (nth history current-step)

        winner
        (determine-winner squares)

        jump-to
        #(set-state assoc :current-step %)

        moves
        (map-indexed
          (fn [i step]
            (d/li
              {:key i}
              (d/button
                {:on-click #(jump-to i)}
                (let [text (if (zero? i)
                             "Go to game start"
                             (str "Go to move #" i))]
                  (if (= current-step i)
                    (d/strong text)
                    text)))))
          history)

        status
        (cond
          winner
          (str "Winner: " winner)

          (tie? squares)
          "It's a tie!"

          :else
          (str "Next player: " (current-player current-step)))

        handle-click
        (fn [i]
          (let [history           (->> history
                                       (take (inc current-step))
                                       vec)
                {:keys [squares]} (last history)
                winner            (determine-winner squares)]
            (when-not (or winner (get squares i))
              (set-state
                {:history
                 (conj history
                       {:squares (assoc squares i
                                        (current-player current-step))})

                 :current-step
                 (count history)}))))]

    (d/div
      {:class "game"}
      (d/div
        {:class "game-board"}
        ($ board {:squares  squares
                  :on-click handle-click}))
      (d/div {:class "game-info"}
        (d/div status)
        (d/ol moves)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn init
  []
  (rdom/render
    ($ game)
    (js/document.getElementById "app")))

