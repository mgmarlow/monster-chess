(local board { :width 5 :height 5 :square-size 64
               :colors { :light [0.9 0.9 0.8]
                         :dark [0.5 0.4 0.3] } })

(local wpawn-img (love.graphics.newImage "img/wP.png"))
(local bpawn-img (love.graphics.newImage "img/bP.png"))
(local wknight-img (love.graphics.newImage "img/wN.png"))
(local bknight-img (love.graphics.newImage "img/bN.png"))
(local wbishop-img (love.graphics.newImage "img/wB.png"))
(local bbishop-img (love.graphics.newImage "img/bB.png"))
(local wrook-img (love.graphics.newImage "img/wR.png"))
(local brook-img (love.graphics.newImage "img/bR.png"))
(local wqueen-img (love.graphics.newImage "img/wQ.png"))
(local bqueen-img (love.graphics.newImage "img/bQ.png"))
(local wking-img (love.graphics.newImage "img/wK.png"))
(local bking-img (love.graphics.newImage "img/bK.png"))

(fn offset-x []
  (- (/ (love.graphics.getWidth) 2)
     (* (/ (. board :width) 2) (. board :square-size))))

(fn offset-y []
  (- (/ (love.graphics.getHeight) 2)
     (* (/ (. board :height) 2) (. board :square-size))))

;; (local player-knight { :x 0 :y 0 })
;; (local player-bishop { :x 0 :y 0 })
;; (local player-rook { :x 0 :y 0 })
(local player-king { :row 5 :col 3 :img wking-img })

(fn draw-img [img row col]
  "Translates row/col to pixel coordinates and account for middle-of-screen offset."
  ;; 64-50 = 14, 14/2 = 7, might parameterize later
  (let [x (+ (* col (. board :square-size)) (offset-x) 7)
        y (- (+ (* row (. board :square-size)) (offset-y)) (+ (img:getWidth) 7))]
  (love.graphics.draw img x y)))

(fn draw-piece [piece]
  (draw-img (. piece :img) (. piece :row) (. piece :col)))

(fn draw-board []
  (for [row 0 (- (. board :height) 1)]
    (for [col 0 (- (. board :width) 1)]
      (let [x (* col (. board :square-size))
            y (* row (. board :square-size))
            light (= (% (+ row col) 2) 0)]
        (love.graphics.setColor
         (unpack (if light
                     (. board :colors :light)
                     (. board :colors :dark))))
        (love.graphics.rectangle
         "fill"
         (+ x (offset-x)) (+ y (offset-y))
         (. board :square-size) (. board :square-size))))))

(fn love.load []
  (love.window.setTitle "kingslayer"))

(fn love.draw []
  (draw-board)
  (draw-piece player-king))

(fn love.update [dt])
