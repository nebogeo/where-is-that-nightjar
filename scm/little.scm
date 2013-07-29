(define (choose l)
  (list-ref l (random (length l))))

(define (transform x y r s) (list x y r s))

(define (transform-x t) (list-ref t 0))
(define (transform-y t) (list-ref t 1))

(define image-lib ())

(define (load-image! fn finished)
  (let ((image (js "new Image()")))
    (set! image.onload
          (lambda ()
            (set! image-lib (cons (list fn image) image-lib))
            (when (eq? (length image-lib)
                       (length images))
                  (finished))))
    (set! image.src (+ "images/" fn ".png"))))

(define (load-images! l finished)
  (for-each
   (lambda (fn)
     (load-image! fn finished))
   l))

(define (find-image fn l)
  (cond
   ((null? l) #f)
   ((eq? (car (car l)) fn) (cadr (car l)))
   (else (find-image fn (cdr l)))))

;; ----------------------------------------

(define (centre-text ctx txt y)
  (let ((m (ctx.measureText txt)))
    (ctx.fillText txt (- (/ screen-width 2) (/ m.width 2)) y)))

;; ----------------------------------------

(define (rect-button x y w h callback)
  (list "rect-button" x y w h callback))

(define (circle-button x y r callback)
  (list "circle-button" x y r r callback))

(define (button-type b) (list-ref b 0))
(define (button-x b) (list-ref b 1))
(define (button-y b) (list-ref b 2))
(define (button-w b) (list-ref b 3))
(define (button-r b) (list-ref b 3))
(define (button-h b) (list-ref b 4))
(define (button-callback b) (list-ref b 5))

(define (dist-2d x1 y1 x2 y2)
  (let ((x (- x2 x1))
        (y (- y2 y1)))
    (Math.sqrt (+ (* x x) (* y y)))))

(define (in-rect? x y w h xx yy)
  (and (> xx x)
       (< xx (+ x w))
       (> yy y)
       (< yy (+ y h))))
  
(define (in-circle? x y r xx yy)
  (< (dist-2d xx yy x y) r))

(define (rect-button-update! b mx my c)
  (if (in-rect? (button-x b) (button-y b)
                (button-w b) (button-h b)
                mx my)
      (let ((fn (button-callback b)))
        (fn c))
      c))

(define (circle-button-update! b mx my c)
  (if (in-circle? (button-x b) (button-y b)
                  (button-r b) mx my)
      (let ((fn (button-callback b)))
        (fn c))
      c))

(define (button-update! b mx my c)
  (if (eq? (button-type b) "rect-button")
      (rect-button-update! b mx my c)
      (circle-button-update! b mx my c)))

(define (button-render! ctx b)
  (if (eq? (button-type b) "rect-button")      
      (ctx.strokeRect 
       (button-x b)
       (button-y b)
       (button-w b)
       (button-h b))
      (begin
        (ctx.beginPath)
        (ctx.arc (button-x b) (button-y b)
                 (button-r b) 0 Math.PI*2 true)
        (ctx.closePath)
        (ctx.stroke))))

;; ----------------------------------------

(define (button-list b)
  b)

(define (buttons-update b mx my c)
  (foldl
   (lambda (b r)
     (button-update! b mx my r))
   c b))

(define (buttons-render! ctx b)
  (for-each
   (lambda (b)
     (button-render! ctx b))
   b))

;; ----------------------------------------

(define (make-new-game) 
  (list 0  
        (lambda (ctx) 
          0) 
        (lambda (t c)
          c)
        ()
        0
        0
        ()))

(define (game-time g) (list-ref g 0))
(define (game-modify-time v g) (list-replace g 0 v))
(define (game-render g) (list-ref g 1))
(define (game-modify-render v g) (list-replace g 1 v))
(define (game-update g) (list-ref g 2))
(define (game-modify-update v g) (list-replace g 2 v))
(define (game-buttons g) (list-ref g 3))
(define (game-modify-buttons v g) (list-replace g 3 v))
(define (game-data g) (list-ref g 4))
(define (game-modify-data fn g) (list-replace g 4 (fn (game-data g))))

(define (game-input g mx my)
  (buttons-update (game-buttons g) mx my g))

;; ----------------------------------------

(define (top-update-game t game)
  (let ((fn (game-update game)))
    (set! game (game-modify-time
                t (fn t game)))))

(define (top-render-game ctx game)
  (let ((fn (game-render game)))
    (fn ctx)))

(define (top-render)
  (when (not (eq? game 0))
        (ctx.clearRect 0 0 screen_width screen_height)
        (let ((t (- (js "new Date()") load-time)))
          (set! ctx.font "bold 10pt Courier")
          (ctx.fillText (+ "Time is: " t) 10 460)
          (set! ctx.font "bold 20pt Courier")
        
          (top-update-game t game)
          (top-render-game ctx game)
          (buttons-render! ctx (game-buttons game)))
        (requestAnimFrame top-render ctx)))

(define game 0)

(define (start-game canvas ctx)
  (ctx.clearRect 0 0 screen_width screen_height)
  (canvas.addEventListener 
   "mousedown" 
   (lambda (e) 
     (let ((rect (canvas.getBoundingClientRect)))
       (let ((mx (- e.clientX rect.left))
             (my (- e.clientY rect.top)))
         (set! game (game-input game mx my))))))

  (console.log "building game")
  ;; todo - pass in game specific func
  (set! game (nightjar-intro (make-new-game)))
  (requestAnimFrame top-render ctx))

;; ----------------------------------------

(console.log "started nighjar game")

(define canvas (document.getElementById "canvas"))
(define ctx (canvas.getContext "2d"))

(define screen-width 640)
(define screen-height 480)
(define load-time (js "new Date()"))

(set! ctx.fillStyle "#000000")
(set! ctx.strokeStyle "#000000")
(set! ctx.font "bold 20pt Courier")

(centre-text ctx "Loading..." 240)

(console.log "done stuff")

(load-images!
 images
 (lambda ()
   (console.log "ready")
   (start-game canvas ctx)))

