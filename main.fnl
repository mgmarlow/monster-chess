(local board { :width 5 :height 5 :square-size 64
               :colors { :light [0.9 0.9 0.8]
                         :dark [0.5 0.4 0.3] } })

(fn draw-board []
  (for [row 0 (- (. board :height) 1)]
    (for [col 0 (- (. board :width) 1)]
      (let [offset-x (- (/ (love.graphics.getWidth) 2)
                        (* (/ (. board :width) 2) (. board :square-size)))
            offset-y (- (/ (love.graphics.getHeight) 2)
                        (* (/ (. board :height) 2) (. board :square-size)))
            x (* col (. board :square-size))
            y (* row (. board :square-size))
            light (= (% (+ row col) 2) 0)]
        (love.graphics.setColor
         (unpack (if light
                     (. board :colors :light)
                     (. board :colors :dark))))
        (love.graphics.rectangle
         "fill" (+ x offset-x) (+ y offset-y) (. board :square-size) (. board :square-size))))))

(fn love.load []
  (love.window.setTitle "kingslayer"))

(fn love.draw []
  (draw-board))

(fn love.update [dt])
