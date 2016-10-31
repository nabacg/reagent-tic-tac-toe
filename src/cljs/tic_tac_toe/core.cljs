(ns tic-tac-toe.core
    (:require [reagent.core :as reagent :refer [atom]]
              [reagent.session :as session]
              [secretary.core :as secretary :include-macros true]
              [accountant.core :as accountant]))

;; Logic - should go into cljx
(defn initial-state [size]
   {:size size
    :current-player :player-1
    :player-1 #{}
    :player-2 #{}})

(def game-state (atom (initial-state 4)))

(defn h-consecutive?
  "horizontally-consecutive?" [[x1 y1] [x2 y2]]
  (and (= x1 (dec x2))
       (= y1 y2)))

(defn v-consecutive?
  "vertically-consecutive?"
  [[x1 y1] [x2 y2]]
  (and (= x1 x2)
       (= y1 (dec  y2))))

(defn d-consecutive?
  "diagonally-consecutive?"
  [[x1 y1] [x2 y2]]
  (and (= x1 (dec x2))
       (= y1 (dec y2))))

(defn inv-d-consecutive?
  "inverted=diagonally-consecutive?"
  [[x1 y1] [x2 y2]]
  (and (= x1 (dec x2))
       (= (dec  y1)  y2)))

(defn longest-consecutive [sorted-points]
  (let [consecutive-counter
        #(->> (map % sorted-points (rest sorted-points))
              (reduce (fn [c e] (if e (inc c) 0)) 0 ))]
    (->> [h-consecutive? v-consecutive? d-consecutive? inv-d-consecutive?]
         (map consecutive-counter)
         (apply max))))

(defn end-game? [{:keys [size player-1 player-2]}]
  (let [p1s (sort player-1)
        p2s (sort player-2)]
    (cond
      (= (dec size)
         (longest-consecutive (sort player-1))) :player-1
      (= (dec size)
         (longest-consecutive (sort player-2))) :player-2
      :else nil)))

(defn flip-current-player [old-player]
  (if (=  old-player :player-1)
    :player-2
    :player-1))

(defn handle-click [state-atom [x y :as point]]

  (fn [e]
    (println "Clicked x=" x " ,y=" y)
    (swap! state-atom (fn [{:keys [current-player player-1 player-2] :as old-state}]
                        (if (contains? (old-state current-player) point)
                          (do
                            (println "this point is taken by player=" current-player)
                             old-state)
                          (do
                            (println "Changing state old-state=" old-state)
                            (-> old-state
                                   (update current-player conj point)
                                   (update :current-player flip-current-player))
                               )
                          )
                        ))
    ))

(defn init-game-state! []
  (swap! game-state (fn [_] (initial-state 3))))

(defn display-state [{:keys [size player-1 player-2] :as state}]
  (if-let [winner (end-game? state)]
    [:div
     [:h3  "AND THE WINNER IS "]
     [:h1  (name  winner)]
     ]
    (->>  (for [y (range size) ]
            (->> (range 0 size)
                 (map (fn [x]
                                        ;todo this shouldn't really refer to global..
                        (into [:td
                               {:on-click (handle-click game-state [x y])
                                :style {:cursor :pointer}
                                }
                               ]
                              (cond
                                (contains? player-1 [x y]) ["x"]
                                (contains? player-2 [x y]) ["o"]
                                :else ["_"]))))

                 (into [:tr ] )))
          (into  [:table])
          (conj [:div
                 [:div  [:p "Current player:"] [:p (:current-player state)]] ]))))




;; -------------------------
;; Views

(defn home-page []
  [:div [:h2 "Tic-Tac-Toe"]
   (display-state @game-state)
   [:a {:on-click (fn [e] (init-game-state!))}  "Restart?"]
   ])

(defn about-page []
  [:div [:h2 "About tic-tac-toe"]
   [:div [:a {:href "/"} "go to the home page"]]])

(defn current-page []
  [:div [(session/get :current-page)]])

;; -------------------------
;; Routes

(secretary/defroute "/" []
  (session/put! :current-page #'home-page))

(secretary/defroute "/about" []
  (session/put! :current-page #'about-page))

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (accountant/configure-navigation!
    {:nav-handler
     (fn [path]
       (secretary/dispatch! path))
     :path-exists?
     (fn [path]
       (secretary/locate-route path))})
  (accountant/dispatch-current!)
  (mount-root))
