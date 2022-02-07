(ns wordle.reactive
  (:require [lambdaisland.thicc :as dom]))

(defonce state (atom {:word "hello"
                      :responses []}))

(defn pad [n coll]
  (concat coll (repeat (- n (count coll)) nil)))

(def board
  (dom/reatom
   (fn [{:keys [word responses]}]
     [:div.board
      (for [row (pad 6 responses)
            [expected letter] (map vector word (pad 5 row))]
        [:div.cell
         {:style {:background-color
                  (cond
                    (= expected letter) "green"
                    (contains? (set word) letter) "orange"
                    :else "#333")}}
         letter])])
   state))

(defn user-input [e]
  (let [l (.toLowerCase (.-key e))]
    (swap! state
           (fn [{:keys [responses] :as state}]
             (if (and (seq responses)
                      (not (= (count (last responses)) 5)))
               (update-in state [:responses (dec (count responses))]
                          conj l)
               (update state :responses conj [l]))))))

(defn ^export mount []
  (conj! (dom/el-by-id "app")
         (dom/dom
          [:div board]))
  (js/document.addEventListener "keydown" user-input))
