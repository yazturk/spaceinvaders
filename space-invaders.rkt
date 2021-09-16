#!/usr/bin/racket
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

(require 2htdp/universe)
(require 2htdp/image)

;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)
(define MISSILE-RATE 100)
(define BACKGROUND (place-image
                    (circle 30 "solid" "yellow")
                    60 60
                    (place-image (rectangle WIDTH 50 "solid" "brown")
                                 (/ WIDTH 2) HEIGHT
                                 (empty-scene WIDTH HEIGHT))))
(define END (overlay
             (text "End of World." 48 "black")
             (rectangle WIDTH HEIGHT "solid" "gray")))
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

(define MISSILE (ellipse 5 15 "solid" "red"))

(define TANK-Y (- HEIGHT (image-height TANK)))

(define-struct tank (x dir))
(define T0 (make-tank (/ WIDTH 2) 0))   ;center going right
(define-struct invader (x y dx))
(define-struct missile (x y))
(define-struct game (invaders missiles t))
(define G0 (make-game empty empty T0))
;; FUNCTIONS =============================================
(define (main g)
  (big-bang g
            (on-tick next-game)     ;Game -> Game
            (to-draw render-game)   ;Game -> Image
            (on-key  key-handle)    ;KeyEvent Game -> Game
            (stop-when they-landed last) ;Game -> Boolean, Game -> Image
            ))

;; ADVANCING GAME FUNCTIONS
(define (next-game g) (collisions (advance-game g)))

(define (advance-game s)
  (make-game
   (add-invader (next-loi (game-invaders s)))
   (next-lom (game-missiles s))
   (next-tank (game-t s))))

(define (add-invader loi)
  (if (or (empty? loi)
          (= (modulo (round (invader-y (first loi))) INVADE-RATE) 0))
      (append loi (cons (make-invader (random WIDTH) (- (random 100)) 1) empty))
      loi))

(define (next-loi loi)
  (cond [(empty? loi) empty]
        [else
         (cons (next-i (first loi) (+ INVADER-Y-SPEED (invader-y (first loi))))
               (next-loi (rest loi)))]))

(define (next-i i y)
  (if (< (invader-dx i) 0)
      (if (<= (invader-x i) INVADER-X-SPEED)
          (make-invader 0 y (* -1 (invader-dx i)))
          (make-invader (- (invader-x i) INVADER-X-SPEED) y (invader-dx i)))
      (if (>= (invader-x i) (- WIDTH INVADER-X-SPEED))
          (make-invader WIDTH y (* -1 (invader-dx i)))
          (make-invader (+ (invader-x i) 1.5) y (invader-dx i)))))

(define (next-lom lom)
  (cond [(empty? lom) empty]
        [else (append (next-missile (first lom)) (next-lom (rest lom)))]))

(define (next-missile m)
  (if (<= (missile-y m) 0)
      empty
      (cons (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)) empty)))
(define (next-tank t)
  (cond [(= (tank-dir t) 0) t]
        [(= (tank-dir t) 1)
         (if (>= (tank-x t) (- WIDTH 2))
             (make-tank WIDTH 0)
             (make-tank (+ TANK-SPEED (tank-x t)) (tank-dir t)))]
        [else
         (if (<= (tank-x t) 2)
             (make-tank 0 0)
             (make-tank (- (tank-x t) TANK-SPEED) (tank-dir t)))]))

(define (add-missile g)
  (if (empty? (game-missiles g))
      (make-game
       (game-invaders g)
       (cons (make-missile
              (tank-x (game-t g))
              TANK-Y) empty)
       (game-t g))
      (if (<= (missile-y (first (game-missiles g))) (- TANK-Y MISSILE-RATE))
          (make-game
           (game-invaders g)
           (cons (make-missile
                  (tank-x (game-t g))
                  TANK-Y) (game-missiles g))
           (game-t g))
          g)))

;; COLLISION FUNCTIONS

(define (collisions g)
  (cond [(empty? (game-missiles g)) g]
        [(empty? (game-invaders g)) g]
        [else (m-coll (first (game-missiles g))
                      (collisions
                       (make-game (game-invaders g)
                                  (rest (game-missiles g))
                                  (game-t g))))]))
(define (m-coll m g)
  (cond [(empty? (game-invaders g))
         (make-game empty
                    (cons m (game-missiles g))
                    (game-t g))]
        [else (if (coll? m (first (game-invaders g)))
                  (make-game (rest (game-invaders g))
                             (game-missiles g)
                             (game-t g))
                  (append-invader (first (game-invaders g))
                                  (m-coll m (make-game
                                             (rest (game-invaders g))
                                             (game-missiles g)
                                             (game-t g)))))]))
(define (append-invader i g)
  (make-game (cons i (game-invaders g))
             (game-missiles g)
             (game-t g)))
(define (coll? m i)
  (<=
   (sqrt (+
          (sqr (- (missile-x m) (invader-x i)))
          (sqr (- (missile-y m) (invader-y i)))))
   HIT-RANGE))

;; RENDER FUNCTIONS
(define (render-game s)
  (place-image
   TANK
   (tank-x (game-t s)) TANK-Y
   (place-invaders (game-invaders s)
                   (place-missiles (game-missiles s) BACKGROUND))))

(define (place-invaders loi bg)
  (cond [(empty? loi) bg]
        [else
         (place-image INVADER
                      (invader-x (first loi))
                      (invader-y (first loi))
                      (place-invaders (rest loi) bg))]))

(define (place-missiles lom bg)
  (cond [(empty? lom) bg]
        [else
         (place-image MISSILE
                      (missile-x (first lom))
                      (missile-y (first lom))
                      (place-missiles (rest lom) bg))]))

(define (key-handle g ke)
  (cond [(key=? " " ke) (add-missile g)]
        [(key=? "left" ke) (make-game (game-invaders g)
                                      (game-missiles g)
                                      (make-tank (tank-x (game-t g))
                                                 (if (= 1 (tank-dir (game-t g))) 0 -1)))]
        [(key=? "right" ke) (make-game (game-invaders g)
                                       (game-missiles g)
                                       (make-tank (tank-x (game-t g))
                                                  (if (= -1 (tank-dir (game-t g))) 0 1)))]
        [else g]))

(define (they-landed g)
  (if (empty? (game-invaders g))
      false
      (>= (invader-y (first (game-invaders g))) HEIGHT)))

(define (last g) END)

(main G0) ;GO!
