(ns crosscram.bots
  (:require [crosscram.core :as crosscram])
  (:require [crosscram.board :as board])
  (:require [clojure.core.match :as match])
  )

;;;
;;; Some naive bots
;;;

(defn brute-force-moves [game]
  (first (board/available-moves game)))

(defn random-moves [game]
  (rand-nth (board/available-moves game)))

;;;
;;; Common Utility Methods
;;;

(defn- maximize-available-moves [scorefn game]
  (first (reverse (sort-by
      (fn [arg] (scorefn arg game))
      (board/available-moves game)))))

;;;
;;; Better Than Random
;;;

(defn- num-neighbors [[r c] board]
  (+  (if-not (board/valid-empty-space? board [r (inc c)]) 1 0)
      (if-not (board/valid-empty-space? board [r (dec c)]) 1 0)))

(defn- distance-from-opponent [[pos-a pos-b] game]
  (+  (num-neighbors pos-a (:board game))
      (num-neighbors pos-b (:board game))))

(defn- score-for-avoiding-crowded-spaces [[r c] board]
  (let [arr (map (fn [row] (get row c)) board)]
  (+
    (cond
      (= (+ r 1) (count arr)) 0
      (not (nil? (nth arr (+ r 1)))) 0
      (= (+ r 2) (count arr)) 2
      (not (nil? (nth arr (+ r 2)))) 2
      :else 1)
    (cond
      (= (- r 1) -1) 0
      (not (nil? (nth arr (- r 1)))) 0
      (= (- r 2) -1) 2
      (not (nil? (nth arr (- r 2)))) 2
      :else 1))))

(defn- distance-from-self [[pos-a pos-b] game]
  (+ (score-for-avoiding-crowded-spaces pos-a (:board game))
    (score-for-avoiding-crowded-spaces pos-b (:board game))))

(defn- score [piece game]
  (+ (distance-from-self piece game) (distance-from-opponent piece game)))

(defn better-than-random [game]
  (maximize-available-moves score game))

;;;
;;; Maximize Next Turn's Point Spread
;;;

(defn- point-spread [[pos-a pos-b] game]
  (let [nextgame (assoc game :board (board/add-piece game 0 pos-a pos-b))]
    (- (count (board/available-moves nextgame))
      (count (board/available-opposing-moves nextgame)))))

(defn maximize-next-turns-point-spread [game]
  (maximize-available-moves point-spread game))

;;;
;;; Main
;;;

(defn -main [rows columns num-games]
  (println (str "Scores: "
    (crosscram/play-symmetric
      (crosscram/new-game (Integer/parseInt rows) (Integer/parseInt columns) :horizontal )
      random-moves
      maximize-next-turns-point-spread
      (Integer/parseInt num-games)))))
