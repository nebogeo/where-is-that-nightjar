#lang racket
(require (planet jaymccarthy/sqlite:5:1/sqlite))
(provide (all-defined-out))


(define (setup db)
  (exec/ignore db "CREATE TABLE player ( id INTEGER PRIMARY KEY AUTOINCREMENT, species TEXT, played_before INTEGER, age_range INTEGER )")
  (exec/ignore db "CREATE TABLE click ( id INTEGER PRIMARY KEY AUTOINCREMENT, player_id INTEGER, photo_name TEXT, photo_offset_x INTEGER, photo_offset_y INTEGER, time_stamp INTEGER, x_position INTEGER, y_position INTEGER, success INTEGER )")
  )

(define (insert-player db species played_before age_range)
  (insert db (string-append
              "INSERT INTO player VALUES (NULL, '"
              species "', '"
              (if (equal? played_before "false") "0" "1") "', '"
              age_range "')")))

(define (insert-click db player_id photo_name photo_offset_x photo_offset_y time_stamp x_position y_position success)
  (insert db (string-append
              "INSERT INTO click VALUES (NULL, '"
              player_id "', '"
              photo_name "', '"
              photo_offset_x "', '"
              photo_offset_y "', '"
              time_stamp "', '"
              x_position "', '"
              y_position "', '"
              success "')")))

(define (get-player-averages db)
  (let ((players (cdr (select db "SELECT * from player"))))
    (filter
     (lambda (av)
       (not (false? av)))
     (map
      (lambda (player)
        (get-player-average db (vector-ref player 0)))
      players))))

(define (get-player-average db player-id)
  (vector-ref
   (cadr
    (select db (string-append
                "SELECT avg(time_stamp) from click where player_id = "
                (number->string player-id)))) 0))
