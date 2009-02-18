;;package org.newdawn.slick.examples.scroller;
;; Apparently there is no way to import all the
;; classes from a java package ... which kinda sucks.
(ns tank
  (:import (org.newdawn.slick Animation
                              AppGameContainer
                              BasicGame
                              GameContainer
                              Graphics
                              Input
                              SlickException
                              SpriteSheet)
           (org.newdawn.slick.tiled TiledMap)
           (org.newdawn.slick.util Log)))

;; Constants

(def tank-size 32)
(def tile-size 32)
(def tank-move-speed 0.003)
(def tank-rotate-speed 0.2)

;; Structure for the player

(defstruct player :x :y :dx :dy :ang :animation)

(defn make-animation
  [images]
  (let [a (new Animation)]
    (dorun
     (for [image images]
       (do
         (.addFrame a image 150))))
    (.setAutoUpdate a false)
    a))

(defn make-images
  [sheet]
  (map (fn [idx] (.getSprite sheet idx 1))
       (range 0 7)))

(defn resrc
  [name]
  (str "./src/main/resources/" name))

(defn load-player-animation
  "Load the animation of a player"
  []
  (let [sheet (new SpriteSheet (resrc "sprites.png") 32 32)]
    (make-animation (make-images sheet))))

(defn make-player
  "Initialize the player"
  []
  (struct player
          15 16
          0.0 0.0 0.0
          (load-player-animation)))

(defn update-player-movement-vector
  "Changes the direction of the player based on its new angle"
  [player]
  (let [radian-ang (Math/toRadians (player :ang))]
    (assoc player
      :dx (float (Math/sin radian-ang))
      :dy (float (- (Math/cos radian-ang))))))

;; Structure for the screen and colision detection

(defstruct screen :w :h :top-offset :left-offset :map :blocked)

;; Collision map
;; This is the part I am less happy about.
;; I had to recreate a bunch of utility functions
;; to create a matrix, and then I getting
;; things in a table .. I am pretty sure
;; I could have done better.
;; Maybe using only a one dimension array ?
(defn blocked?
  "Is a position blocked in the screen?"
  [screen x y]
  (let [i (int x)
        j (int y)]
    (true? (get (screen :blocked) [i,j]))))

;; All matrices positions
(defn make-couples [i j]
        (for [x (range 0 j)
                    y (range 0 i)]
                 [x,y]))
(defn genmap
  [keys f]
  (zipmap keys (map f keys)))

(defn tile-blocked?
        "Is a tile blocked in a tile map ?"
        [tile-map i j]
        (let [tileId (.getTileId tile-map i j 0)]
     (Boolean/parseBoolean
       (.getTileProperty tile-map tileId "blocked" "false"))))

(defn make-collision-map
  "Builds a double dimensionned array telling
        whether a cell is blocked"
  [tile-map w h]
  (genmap (make-couples w h)
          (fn [couple]
            (let [[i,j] couple]
              (tile-blocked? tile-map i j)))))

;;;

(defn make-screen
  "Make the screens, prepare the map of blocked cells"
  [container]
  (let [w (float (/ (.getWidth container) tile-size))
        h (float (/ (.getHeight container) tile-size))
        map (new TiledMap (resrc "map.tmx"))]
    { :w w
     :h h
     :top-offset (int (/ h 2))
     :left-offset (int (/ w 2))
     :map map
     :blocked (make-collision-map map (.getWidth map) (.getHeight map)) }))

;; Movement
(defn try-move
  "Try and move a player in the screen given a direction.
                Returns a list with the moved player, and a
                boolean indicating whether the move was successfull.
        "
  [player screen dx dy]
  (let [new_x (+ (player :x) dx)
        new_y (+ (player :y) dy)]
                (cond
                  (not (blocked? screen new_x new_y)) [(assoc player :y new_y :x new_x) true]
                  (not (blocked? screen new_x (player :y))) [(assoc player :x new_x) true]
                  (not (blocked? screen (player :x) new_y)) [(assoc player :y new_y) true]
                  :else [player false])))

(defn key-down?
  "Is a key down on a container ?"
  [container key]
  (.isKeyDown (.getInput container) key))

(defn turn
  "Changes the angle of the player"
  [player direction delta]
  (let [new-angle (+ (player :ang)
                     (* direction delta tank-rotate-speed))]
    (assoc  player :ang new-angle)))

(defn update-player-angle
  "Update the angle of a player based on the container's input"
  [player container delta]
  (if (key-down? container Input/KEY_RIGHT)
    (update-player-movement-vector (turn player 1 delta))
    (if (key-down? container Input/KEY_LEFT)
      (update-player-movement-vector (turn player (- 1) delta))
      player)))

(defn update-player-animation
  "Changes the animation of a player for a given delta of time"
  [player delta]
  (let [a (player :animation)]
    (.update a delta)
    player))

(defn update-player-position
  "Changes the position and the animation of the player if possible"
  [player screen container delta]
  (let [dx (* (player :dx) delta tank-move-speed)
        dy (* (player :dy) delta tank-move-speed)]

    (let [[p,moved]
          (if (key-down? container Input/KEY_UP)
            (try-move player screen dx dy)
            (if (key-down? container Input/KEY_DOWN)
              (try-move player screen (- dx) (- dy))
              [player, false]))]
      (if moved
        (update-player-animation p delta)
        p))))


;; ------------------------------------------------------

;; Rendering functions (ugly as possible...)
(defn render-draw-map
  "Draws only the relevant part of the map"
  [player screen]

  (let [i_px (int (player :x))
        i_py (int (player :y))]
    (let [o_px (int (* (- i_px (player :x)) tile-size))
          o_py (int (* (- i_py (player :y)) tile-size))
          mx (- o_px (/ tank-size 2))
          my (- o_py (/ tank-size 2))
          m_sx (int (- i_px (screen :left-offset) 1.0))
          m_sy (int (- i_py (screen :top-offset) 1.0))
          m_w (int (+ (screen :w) 3))
          m_h (int (+ (screen :h) 3))]
      (.render (screen :map) mx my m_sx m_sy m_w m_h)))
  nil)

(defn render-translate-graphics
  "Draw entities relative to the screen"
  [graphics player]
  (let [x (- 400 (int (* (player :x) 32)))
        y (- 300 (int (* (player :y) 32)))]
    (.translate graphics x y))
  nil)

(defn render-draw-tank
  "Draws the tank"
  [graphics player screen]
  (let [cx (int (* 32 (player :x)))
        cy (int (* 32 (player :y)))
        rot (player :ang)]
    (.rotate graphics cx cy rot)
    (.draw (player :animation) (- cx 16) (- cy 16))
    (.rotate graphics cx cy rot))
  nil)

;; Mutable state
(defn reset-game [player screen container]
  (dosync
   (ref-set player (update-player-movement-vector (make-player)))
   (ref-set screen (make-screen container)))
  nil)

;; Mutation function
(defn update-game-model
  "Function used by alter to move the player around"
  [player screen container delta]
  (update-player-position
   (update-player-angle player container delta)
   screen container delta))

;; Proxy for the basic game class
(def scroller
     (let [player (ref nil)
           screen (ref nil)]
       (proxy [BasicGame] ["Scroller"]
         (init [container]
               (reset-game player screen container)
               )

         (update [container delta]
                 (dosync
                  (alter player
                         update-game-model
                         @screen container delta)))

         (render [container graphics]
                 (render-draw-map @player @screen)
                 (render-translate-graphics graphics @player)
                 (render-draw-tank graphics @player @screen)
                 (.resetTransform graphics)))))

;; Let the fun begin !!
(.start (new AppGameContainer scroller 800 600 false))

