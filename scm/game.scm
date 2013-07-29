; ------------------------------------------------
; nightjar specific stuff

;; get from image structure
(define image-width 900)
(define image-height 600)
(define image-centre-x (/ image-width 2))
(define image-centre-y (/ image-height 2))

(define safe-x 0.2)
(define safe-y 0.2)

(define (generate-image-pos)
  (list (* screen-width (+ safe-x (* (rndf) (- 1 (* safe-x 2)))))
        (* screen-height (+ safe-y (* (rndf) (- 1 (* safe-y 2)))))))

(define default-button-x (- (/ screen-width 2) 100))
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
       (nightjar-modify-images 
        images d))
     (game-modify-data
      (lambda (d)
        (nightjar-modify-image-pos
         (generate-image-pos) d))
      c)))))

(define (nightjar-intro c)
  (game-modify-data
   (lambda (d) 
     (empty-nightjar-data)) 
   (game-modify-render
    (lambda (ctx) 
      (centre-text ctx "Flashy intro!" 240))
    (game-modify-buttons  
     (list 
      (image-button 
       default-button-x 
       default-button-y 
       #t 
       (lambda (c)
         (nightjar-setup c))))
     c))))

(define (nightjar-setup c)
  (game-modify-render
   (lambda (ctx) 
     (centre-text ctx "This is the setup!" 240))
   (game-modify-buttons  
    (list 

     (image-button 
      default-button-x 
      default-button-y  
      #t 
      (lambda (c)
        (nightjar-new-game c)))

     (circle-button 
      150 70 25  
      (lambda (c)
        (nightjar-intro c))))
    c)))

(define (nightjar-game c)
  (game-modify-render
   (lambda (ctx) 
     (let ((name (car (nightjar-images (game-data c)))))
       (ctx.drawImage 
        (find-image name image-lib) 
        (- (car (nightjar-image-pos (game-data c)))
           image-centre-x)
        (- (cadr (nightjar-image-pos (game-data c)))
           image-centre-y))
       (centre-text ctx "This is the game!" 240)))
   (game-modify-buttons  
    (list 

     ;; big lose button over whole screen
     (rect-button 
      0 0 screen-width screen-height #f
      (lambda (c)
        (nightjar-lose 
         (game-modify-data
          (lambda (d)
            (nightjar-modify-score
             (- (game-time c) (nightjar-start-time d)) d))
          c))))

     ;; circle button over nightjar
     (circle-button 
      (car (nightjar-image-pos (game-data c)))
      (cadr (nightjar-image-pos (game-data c)))
      25 
      (lambda (c)
        (nightjar-win          
         (game-modify-data
          (lambda (d)
            (nightjar-modify-score
             (- (game-time c) (nightjar-start-time d)) d))
          c))))
     )    c)))

(define (nightjar-win c)
  (game-modify-render
   (lambda (ctx) 
     (centre-text ctx "Well done, YOU FOUND THE NIGHTJAR!" 240)
     (centre-text 
      ctx 
      (+ "in " 
         (/ (nightjar-score (game-data c)) 1000)
         " seconds")
      270))
   (game-modify-buttons  
    (list 
     (image-button 
      default-button-x 
      (+ default-button-y 50) #t
      (lambda (c)
        (nightjar-new-game c)))
     )    c)))

(define (nightjar-lose c)
  (game-modify-render
   (lambda (ctx) 
     (let ((name (car (nightjar-images (game-data c)))))
       (centre-text ctx "YOU LOSE!" 240)))
   (game-modify-buttons  
    (list 
     
     ;; try another
     (image-button 
      (+ default-button-x 150)
      default-button-y #t
      (lambda (c)
        (nightjar-new-game c)))
 
     ;; try again
     (image-button 
      (- default-button-x 150)
      default-button-y #t
      (lambda (c)
        (nightjar-game
         (game-modify-data
          (lambda (d)
            (nightjar-modify-start-time
             (game-time c) d))
          c))))

     )    c)))


