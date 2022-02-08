(ns wordle.core2
  (:require [lambdaisland.thicc :as dom]))

(def board-state (atom []))
(def counter (atom 0))
(def attempt (atom 0))
(def word-of-the-day (atom "hello"))

(defn write-letter [cell letter]
  (set! (.-textContent cell) letter))

(declare user-input)

(defn make-board [n]
  (let [board (dom/dom
               [:div.board
                {:on-keydown
                 (fn [e]
                   (user-input (.toLowerCase (.-key e))))}
                (for [_ (range n)]
                  [:div.cell])])]
    (reset! board-state (vec (dom/query-all board ".cell")))
    board))

(defn get-letter [cell]
  (.-textContent cell))

(defn color-cell [idx cell]
  (let [color (fn [el color]
                (set! (-> el .-style .-backgroundColor)
                      color))
        letter (get-letter cell)]
    (cond
      (= letter (nth @word-of-the-day idx))
      (color cell "green")

      (contains? (set @word-of-the-day) letter)
      (color cell "aqua")

      :else
      (color cell "#333333"))))

(defn check-solution [cells]
  (doseq [[idx cell] (map-indexed vector cells)]
    (color-cell idx cell))
  (= (mapv get-letter cells)
     (vec @word-of-the-day)))

(defn user-input [key]
  (let [start (* 5 @attempt)
        end (* 5 (inc @attempt))]
    (cond
      (and (re-matches #"[a-z]" key)
           (< @counter end))
      (do
        (write-letter (nth @board-state @counter) key)
        (swap! counter inc))

      (and (= key "backspace")
           (> @counter start))
      (do
        (swap! counter dec)
        (write-letter (nth @board-state @counter) ""))

      (and (= key "enter")
           (= @counter end))
      (do
        (when (check-solution (subvec @board-state start end))
          (js/alert "You won"))
        (swap! attempt inc)))))

(defn ^:dev/after-load reload []
  (dom/replace-child (dom/el-by-id "app")
                     (dom/query ".board")
                     (make-board 30)))


(defn ^export mount []
  (conj! (dom/el-by-id "app")
         (make-board 30))
  (js/document.addEventListener
   "keydown"
   (fn [e]
     (user-input (.toLowerCase (.-key e))))))


(comment
  (do
    (def sim ["a" "a" "a" "a" "a" "enter"
              "e" "h" "o" "l" "o"
              "backspace" "k" "enter"
              "h" "e" "l" "l" "o" "enter"])
    (run! user-input sim)))

;; :thanks 👾
