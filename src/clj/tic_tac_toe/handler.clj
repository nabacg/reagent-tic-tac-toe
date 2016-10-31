(ns tic-tac-toe.handler
  (:require [compojure.core :refer [GET defroutes]]
            [compojure.route :refer [not-found resources]]
            [hiccup.page :refer [include-js include-css html5]]
            [tic-tac-toe.middleware :refer [wrap-middleware]]
            [config.core :refer [env]]))


(def game-state (atom {:size 3
                       :player-1 #{[1 1][2 2] [3 3]}
                       :player-2 #{[1 2]}}))


(defn consecutive? [[x1 y1] [x2 y2]]
  (or
   (and (= x1 (dec x2))
        (= y1 y2))
    (and (= x1 x2)
         (= y1 (dec y2)))
    (and (= x1 (dec x2))
         (= y1 (dec y2)))))

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

                                        ; [[1 3] [2 2] [3 1]]



(defn points-sorted [xs]
  (sort-by (juxt first second) xs))

(defn all-consecutive? [sorted-points]
  (filter identity (map consecutive? sorted-points (rest sorted-points))))

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



(defn print-state [{:keys [size player-1 player-2]}]
  (->>  (for [y (range size) ]
          (->> (range 0 size)
               (map (fn [x]
                      (cond
                        (contains? player-1 [x y]) "x"
                        (contains? player-2 [x y]) "o"
                        :else  "_")))
               (clojure.string/join " ")
               (#(str "[" %  "]"))))
        (clojure.string/join "\n")))



(def mount-target
  [:div#app
      [:h3 "ClojureScript has not been compiled!"]
      [:p "please run "
       [:b "lein figwheel"]
       " in order to start the compiler"]])

(defn head []
  [:head
   [:meta {:charset "utf-8"}]
   [:meta {:name "viewport"
           :content "width=device-width, initial-scale=1"}]
   (include-css (if (env :dev) "/css/site.css" "/css/site.min.css"))])

(defn loading-page []
  (html5
    (head)
    [:body {:class "body-container"}
     mount-target
     (include-js "/js/app.js")]))


(defroutes routes
  (GET "/" [] (loading-page))
  (GET "/about" [] (loading-page))

  (resources "/")
  (not-found "Not Found"))

(def app (wrap-middleware #'routes))
