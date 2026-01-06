;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter-my-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit I1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit I1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit I1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; My space invaders program


;; Game -> Game
;; start the world with ...
;; 
(define (main game)
  (big-bang game                   ; Game
            (on-tick   tock)     ; Game -> Game
            (to-draw   render)   ; Game -> Image
            (on-key move-tank)))    ; Game KeyEvent -> Game

;; Game -> Game
;; produce the next game
;; - randomness will be tested in composed functions
(define (tock game)
  (handle-collisions
   (make-game (advance-missles (game-missiles game))
              (advance-invaders (game-invaders game))
              (game-tank game))))

;; ListOfMissles -> ListOfMissles
;; advance missles fired from tank
(check-expect (advance-missles empty) empty)
(check-expect (advance-missles (list (make-missile 150 300)))
              (list (make-missile 150 (+ 300 MISSILE-SPEED))))

;(define (advance-missles ms) ms) ;stub
(define (advance-missles ms)
  (cond [(empty? ms) ms]
        [else
         (if (off-screen? (first ms))
              (advance-missles (rest ms))
              (cons (advance-missile (first ms))
                    (advance-missles (rest ms))))]))

;; Missile -> Boolean
;; return true if missile less than 0 y coord
(check-expect (off-screen? (make-missile 150 -1)) true)
(check-expect (off-screen? (make-missile 150 0)) false)

;(define (off-screen? m) false);
(define (off-screen? m)
  (< (missile-y m) 0))

;; Missile -> Missile
;; advance the missile
;; assume missile is not off screen
(check-expect (advance-missile (make-missile 150 150))
              (make-missile 150 (+ 150 MISSILE-SPEED)))

;(define (advance-missile m) m) ;stub
(define (advance-missile m)
  (make-missile (missile-x m)
       (+ (missile-y m) MISSILE-SPEED)))

;; ListOfInvaders -> ListOfInvaders
;; advance and move the invaders add invaders by invader rate
(check-expect (advance-invaders empty) empty)
(check-expect (advance-invaders (list (make-invader 150 150 
(define (advance-invaders in) in)

;; Game -> Game
;; handle collisions of missiles and invaders
;; !!!
(define (handle-collisions game) game)
;; Game -> Image
;; render ... 
;; !!!
(define (render game) (place-image
                       TANK
                       (tank-x (game-tank game)) HEIGHT
                       BACKGROUND))

;; Game KeyEvent -> Game
;; move the tank with left and right arrow keys
;; !!!
(define (move-tank game ke) game)