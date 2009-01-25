;;package org.newdawn.slick.examples.scroller;

(import '(org.newdawn.slick 
Animation
AppGameContainer
BasicGame
GameContainer
Graphics
Input
SlickException
SpriteSheet
))
(import '(org.newdawn.slick.tiled TiledMap))

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
		(for [i (range 0 8)]
			(do
				(.addFrame a (get images i) 150)))
		(.setAutoUpdate a false)
		a))
	
;; (new Animation images 150 false))

(defn make-images
	[sheet]
	(map (fn [idx] (.getSprite sheet idx 1))
	 		(range 0 8)))

(defn load-player-animation
	"Load the animation of a player"
	[]
	(let [sheet (new SpriteSheet "resources/sprites.png" 32 32)]
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
	(let [ang (player :ang)]
		(assoc :dx (Math/sin (Math/toRadians ang))
					 :dy (- (Math/cos (Math/toRadians ang)))
				player)))	  

;; Structure for the screen and colision detection

(defstruct screen :w :h :top-offset :left-offset :map :blocked)

(defn blocked?
	"Is a position blocked in the screen?"
	[screen x y]
	(let [i y
				j x]
	(get (get (screen :blocked) i) j)))

(defn make-row-generator
	[cell-generator w]
	(fn [i]
		(vec (map (fn [j] (cell-generator i j))
				(range 0 w)))))

(defn make-matrix 
	[w h cell-generator]
	(vec (map (make-row-generator cell-generator w)
		(range 0 h))))

(defn make-collision-map
	"Builds a double dimensionned array telling
	whether a cell is blocked"
	[m w h]
	(make-matrix 
		(fn [i j]
			(let [tileId (.getTileId m i j 0)]
				(Boolean/parseBoolean 
					(.getTileProperty m tileId "blocked" "false"))))))

(defn make-screen
	"Make the screens, prepare the map of blocked cells"
	[container]
	(let [w (float (/ (.getWidth container) tile-size))
			  h (float (/ (.getHeight container) tile-size))
			  map (new TiledMap "resources/map.tmx")]
		{ :w w 
			:h h
			:top-offset (float (/ h 2))
			:left-offset (float (/ w 2))
			:map map
			:blocked (make-collision-map map w h) }))		

;; Movement
(defn try-move
	"Try and move a player in the screen given a direction.
		Returns a list with the moved player, and a 
		boolean indicating whether the move was successfull.
	"
	[player screen dx dy]
	(let [new_x (+ (player :x) dx)
			  new_y (+ (player :y) dy)]
			  
		(let [bxy (blocked? screen new_x new_y)
				  bx (blocked? screen new_x (player :y))
				  by (blocked? screen	(player :x) new_y)]
				
				(if bxy
					(if bx
						(if by
							[player, false]
							[(assoc player :y new_y), true])
						[(assoc player :x new_x), true])
					[(assoc player :x new_x :y new_y), true]))))
					
					
(defn key-down?
	"Is a key down on a container ?"
	[container key]
	(.. container getInput isKeyDown key))

(defn turn
	"Changes the angle of the player"
	[player direction delta]
	(assoc :ang (* direction delta tank-rotate-speed) player))

(defn update-player-angle
	"Update the angle of a player based on the container's input"
	[player container delta]
	(if (key-down? container Input/KEY_RIGHT)
		(update-player-movement-vector (turn player +1 delta))
		(if (key-down? container Input/KEY_LEFT)
			(update-player-movement-vector (turn player -1 delta))
			player)))					  

(defn update-player-animation 
	"Changes the animation of a player for a given delta of time"
	[player delta]
	(let [a (player :animation)]
		(.update a delta)
		(player)))

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

;; Mutable state ?

(defn reset-game [player screen container]
	(dosync 
		(ref-set player (make-player))
		(ref-set screen (make-screen container)))
	nil)

(defn render-draw-map
	"Draws only the relevant part of the map"
	[player screen]
	
	(let [i_px (int (player :x))
				i_py (int (player :y))]
		(let [o_px (int (* (- (i_px (player :x)) tile-size)))
					o_py (int (* (- (i_py (player :y)) tile-size)))
					mx (- o_px (/ tank-size 2))
					my (- o_py (/ tank-size 2))
					m_sx (- i_px (player :left-offset) 1)
					m_sy (- i_py (player :top-offset) 1)
					m_w (+ (player :w) 3)
					m_h (+ (player :h) 3)]
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
			
(def scroller
	(let [player (ref nil)
				screen (ref nil)]
		(proxy [BasicGame] ["Scroller"]
			(init [container]
				(reset-game player screen container))
			
			(update [container delta]
				(alter player 
					(fn [p container delta]
						(update-player-position (update-player-angle container delta) container delta))))		
							
			(render [container graphics]
				(render-draw-map @player @screen)
				(render-translate-graphics graphics @player)
				(render-draw-tank graphics @player @screen)
				(.resetTransform graphics)))))
		
;; -----------------------------------------------------

;; Automated Tests
(use 'clojure.contrib.test-is)
(deftest test-blocked?-checks-in-array
	(let [screen {:blocked [[false false] 
												  [true false] 
												  [false true]]}]
		(is (false? (blocked? screen 0 0)))
		(is (false? (blocked? screen 1 0)))
		(is (true? (blocked? screen 0 1)))
		(is (false? (blocked? screen 1 1)))
		(is (false? (blocked? screen 0 2)))
		(is (true? (blocked? screen 1 2)))))
				
(deftest test-player-is-moved-if-possible
	(let [screen {:blocked [[false false] 
												  [true false] 
												  [false true]]}
			  player {:x 0 :y 0}]
		(let [[moved, success] (try-move player screen 1 0)]
			(and 
				(is (true? success))
				(is (= 1 (moved :x)))
				(is (= 0 (moved :y)))))
		(let [[moved, success] (try-move player screen 0 1)]
			(and 
				(is (true? success))
				(is (= 0 (moved :x)))
				(is (= 0 (moved :y)))))))
					
	
(run-tests)

;; Test data, just in case
;;(def screen {:blocked [ [false false] [true false] [false true] ]})
;;(def p {:x 0 :y 0})

;; Let the fun beggin !!
(.start (new AppGameContainer scroller))

;;(def M (make-matrix 2 3 (fn [i j] [i,j]))) 
