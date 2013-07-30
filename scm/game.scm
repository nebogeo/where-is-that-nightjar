; ------------------------------------------------
; nightjar specific stuff

(define (nightjar-example file size pos)
  (list file size pos))

(define (nightjar-example-file n) (list-ref n 0))
(define (nightjar-example-size n) (list-ref n 1))
(define (nightjar-example-pos n) (list-ref n 2))

;; get from image structure
(define image-width 2474)
(define image-height 1640)
(define image-centre-x (/ image-width 2))
(define image-centre-y (/ image-height 2))

(define nightjar-examples
  (list
   (nightjar-example "N001" 80 (list -150 0))
   (nightjar-example "N003" 160 (list -50 30))
   (nightjar-example "N002" 120 (list -150 30))
   ))

(define safe-x 0.2)
(define safe-y 0.2)

(define (generate-image-pos)
  (list (* screen-width (+ safe-x (* (rndf) (- 1 (* safe-x 2)))))
        (* screen-height (+ safe-y (* (rndf) (- 1 (* safe-y 2)))))))

(define default-button-x (- (/ screen-width 2) 200))
(define default-button-y (+ (/ screen-height 2) 20))

(define (nightjar-data start-time player-id player-type images)
  (list start-time player-id player-type images 0))

(define (empty-nightjar-data)
  (list 0 0 "" () 0))

(define (nightjar-start-time g) (list-ref g 4))
(define (nightjar-modify-start-time v g) (list-replace g 4 v))
(define (nightjar-player-id g) (list-ref g 5))
(define (nightjar-modify-player-id v g) (list-replace g 5 v))
(define (nightjar-images g) (list-ref g 6))
(define (nightjar-modify-images v g) (list-replace g 6 v))
(define (nightjar-image-pos g) (list-ref g 7))
(define (nightjar-modify-image-pos v g) (list-replace g 7 v))
(define (nightjar-score g) (list-ref g 8))
(define (nightjar-modify-score v g) (list-replace g 8 v))

(define (nightjar-new-game c)
  (nightjar-game
   (game-modify-data
    (lambda (d)
      (nightjar-modify-start-time
       (game-time c) d))
    (game-modify-data
     (lambda (d)
       (nightjar-modify-image-pos
        (generate-image-pos) d))
     c))))

(define (nightjar-intro c)
  (game-modify-data
   (lambda (d)
     (empty-nightjar-data))
   (game-modify-render
    (lambda (ctx)
      (centre-text ctx "Where is that" 200)
      (centre-text ctx "nightjar" 290))
    (game-modify-buttons
     (list
      (image-button
       "Start game"
       default-button-x
       default-button-y
       #t
       (lambda (c)
         (play-sound "sound/button.wav")
         (nightjar-setup c))))
     c))))

(define (nightjar-setup c)
  (game-modify-render
   (lambda (ctx)
     (centre-text ctx "experiment" 240))
   (game-modify-buttons
    (list

     (image-button
      "Start playing"
      (+ default-button-x 300)
      default-button-y
      #t
      (lambda (c)
        (play-sound "sound/button.wav")
        (nightjar-new-game
         (game-modify-data
          (lambda (d)
            (nightjar-modify-images
             nightjar-examples d))
          c))))

     (image-button
      "No thanks"
      (- default-button-x 300)
      default-button-y
      (lambda (c)
        (play-sound "sound/button.wav")
        (nightjar-intro c))))
    c)))

(define (nightjar-game c)
  ;; todo: choose and delete

  (define example (car (nightjar-images (game-data c))))

  (game-modify-render
   (lambda (ctx)
     (ctx.drawImage
      (find-image (nightjar-example-file example) image-lib)
      (- (car (nightjar-image-pos (game-data c)))
         image-centre-x)
      (- (cadr (nightjar-image-pos (game-data c)))
         image-centre-y)))

   (game-modify-buttons
    (list

     ;; circle button over nightjar
     (circle-button
      ""
      (+ (car (nightjar-example-pos example))
         (car (nightjar-image-pos (game-data c))))

      (+ (cadr (nightjar-example-pos example))
         (cadr (nightjar-image-pos (game-data c))))

      (nightjar-example-size example)
      (lambda (c)
        (play-sound "sound/found.wav")
        (nightjar-win
         (game-modify-data
          (lambda (d)
            (nightjar-modify-score
             (- (game-time c) (nightjar-start-time d)) d))
          c))))

     ;; big lose button over whole screen
     (rect-button
      ""
      0 0 screen-width screen-height #f
      (lambda (c)
        (nightjar-lose
         (game-modify-data
          (lambda (d)
            (play-sound "sound/notfound.wav")
            (nightjar-modify-score
             (- (game-time c) (nightjar-start-time d)) d))
          c))))


     ) c)))

(define (nightjar-win c)
  (game-modify-render
   (lambda (ctx)
     (define example (car (nightjar-images (game-data c))))

     (ctx.drawImage
      (find-image (nightjar-example-file example) image-lib)
      (- (car (nightjar-image-pos (game-data c)))
         image-centre-x)
      (- (cadr (nightjar-image-pos (game-data c)))
         image-centre-y))

     (centre-text ctx "Nightjar found" 150)
     (centre-text
      ctx
      (+ "in "
         (/ (nightjar-score (game-data c)) 1000)
         " seconds")
      270))

   (game-modify-buttons
    (list
     (image-button
      "More"
      default-button-x
      (+ default-button-y 50) #t
      (lambda (c)
        (play-sound "sound/button.wav")
        (console.log "doing next nightjar")

        ;; check end of game
        (if (eq? (length (nightjar-images (game-data c))) 1)
            (nightjar-intro c)
            (nightjar-new-game
             (game-modify-data
              (lambda (d)
                (nightjar-modify-images
                 (cdr (nightjar-images d)) d))
              c)))))
      )    c)))

(define (nightjar-lose c)
  (game-modify-render
   (lambda (ctx)
     (define example (car (nightjar-images (game-data c))))

     (ctx.drawImage
      (find-image (nightjar-example-file example) image-lib)
      (- (car (nightjar-image-pos (game-data c)))
         image-centre-x)
      (- (cadr (nightjar-image-pos (game-data c)))
         image-centre-y))


     (let ((name (car (nightjar-images (game-data c)))))
       (centre-text ctx "You lose" 240)))
   (game-modify-buttons
    (list

     ;; try another
     (image-button
      "Next Nightjar"
      (+ default-button-x 300)
      default-button-y #t
      (lambda (c)
        (play-sound "sound/button.wav")
        (nightjar-new-game c)))

     ;; try again
     (image-button
      "Try again"
      (- default-button-x 300)
      default-button-y #t
      (lambda (c)
        (nightjar-game
         (game-modify-data
          (lambda (d)
            (play-sound "sound/button.wav")
            (nightjar-modify-start-time
             (game-time c) d))
          c))))

     )    c)))
