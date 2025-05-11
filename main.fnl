(local board { :width 5 :height 5 :square-size 64
               :colors { :light [0.94 0.85 0.71]
                         :dark [0.71 0.53 0.39]
                         :highlight [0.42 0.62 0.35 0.75]} })

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

(var offset-x 0)
(var offset-y 0)

(macro when-let [[name condition] & body]
  `(let [,name ,condition]
     (when ,name
       (do ,(unpack body)))))

(fn includes? [arr value]
  "Returns true if ARR includes VALUE."
  (accumulate [found nil _ item (ipairs arr)]
    (if (or found (= item value)) true found)))

(fn find [arr callback]
  "Returns true if ARR includes VALUE."
  (accumulate [found nil _ item (ipairs arr)]
    (if (or found (callback item)) true found)))

(fn find-index [arr value]
  "Returns the index of VALUE in ARR or nil if not found."
  (var index nil)
  (each [i el (ipairs arr)]
    (when (= el value)
      (set index i)
      (lua "break")))
  index)

(fn attacks [piece]
  (case piece
    {:kind :pawn :color :white} [[-1 -1] [1 -1]]
    {:kind :pawn :color :black} [[-1 1] [1 1]]
    {:kind :knight} [[-2 -1] [-2 1] [-1 -2] [-1 2] [1 -2] [1 2] [2 -1] [2 1]]
    {:kind :bishop} [[-1 -1] [-1 1] [1 -1] [1 1]]
    {:kind :rook} [[0 -1] [0 1] [-1 0] [1 0]] 
    {:kind :queen} [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]
    {:kind :king} []))

(fn moves [piece]
  (case piece
    {:kind :pawn :color :white} [0 -1]
    {:kind :pawn :color :black} [0 1]
    t (attacks piece)))

(fn rays? [piece]
  "Whether the movement of PIECE continues until bounds are reached."
  (includes? [:bishop :rook :queen] (. piece :kind)))

(local state { :turn :player
               :movable [:knight :bishop :rook]
               :occupied {}
               :moves []
               :selected nil })

(fn movable? [piece]
  (includes? state.movable piece.kind))

(fn remove-movable [piece]
  (when-let [index (find-index state.movable piece.kind)]
            (table.remove state.movable index)))

(local all-pieces
       [
        { :row 1 :col 1 :img bpawn-img :kind :pawn :color :black }
        { :row 1 :col 2 :img bpawn-img :kind :pawn :color :black }
        { :row 1 :col 3 :img bpawn-img :kind :pawn :color :black }
        { :row 3 :col 1 :img wknight-img :kind :knight :color :white }
        { :row 3 :col 2 :img wbishop-img :kind :bishop :color :white }
        { :row 3 :col 3 :img wrook-img :kind :rook :color :white }
        { :row 4 :col 2 :img wking-img :kind :king :color :white }
        ])

(fn remove-piece [piece]
  (when-let [index (find-index all-pieces piece)]
            (table.remove all-pieces index)))

(fn serialize [row col]
  (.. row "," col))

(fn regenerate-occupied-squares []
  (let [squares {}]
    (each [_ piece (ipairs all-pieces)]
      (tset squares (serialize (. piece :row) (. piece :col)) piece))
    (set state.occupied squares)))

(fn find-piece [row col] (. state :occupied (serialize row col)))

(fn coords-to-pixels [row col]
  "Convert ROW, COL to [x, y]."
  [(+ (* col (. board :square-size)) offset-x)
   (+ (* row (. board :square-size)) offset-y)])

(fn pixels-to-coords [x y]
  "Convert X, Y to [row, col]."
  [(math.floor (/ (- y offset-y) (. board :square-size)))
   (math.floor (/ (- x offset-x) (. board :square-size)))])

(fn draw-img [img row col]
  "Translates row/col to pixel coordinates and account for middle-of-screen offset."
  ;; 64-50 = 14, 14/2 = 7, might parameterize later
  (let [x (+ (* col (. board :square-size)) offset-x 7)
        y (+ (* row (. board :square-size)) offset-y 7)]
    (love.graphics.setColor 1 1 1)
    (love.graphics.draw img x y)))

(fn draw-piece [piece]
  (draw-img (. piece :img) (. piece :row) (. piece :col)))

(fn within-bounds? [x y]
  (and (>= x 0) (>= y 0) (< x (. board :width)) (< y (. board :height))))

;; TODO: knight bug where movable wraps around at the edges
(fn movable-squares [piece]
  (let [deltas (attacks piece)
        [row col] [piece.row piece.col]
        squares []]
    (each [_ delta (ipairs deltas)]
      (let [[drow dcol] delta]
        (var next-row (+ row drow))
        (var next-col (+ col dcol))

        (while (within-bounds? next-row next-col)
          (let [other-piece (. state :occupied (serialize next-row next-col))]
            (when other-piece
              (when (= (. other-piece :color) :black)
                (table.insert squares [next-row next-col]))
              (lua "break"))
            (table.insert squares [next-row next-col]))
          (set next-row (+ next-row drow))
          (set next-col (+ next-col dcol)))))
    squares))

(fn capture-piece [winner loser]
  (remove-movable winner)
  (set winner.row (. loser :row))
  (set winner.col (. loser :col))
  (remove-piece loser)
  (regenerate-occupied-squares))

(fn move-piece [piece row col]
  (remove-movable piece)
  (set piece.row row)
  (set piece.col col)
  (regenerate-occupied-squares))

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
         (+ x offset-x) (+ y offset-y)
         (. board :square-size) (. board :square-size))))))

(fn draw-highlight [row col]
  (let [[x y] (coords-to-pixels row col)]
    (love.graphics.setColor (unpack (. board :colors :highlight)))
    (love.graphics.rectangle "fill"
                             x y
                             (. board :square-size) (. board :square-size))))

(fn love.load []
  (love.window.setTitle "kingsguard")
  (regenerate-occupied-squares))

(fn love.draw []
  (set offset-x
    (- (/ (love.graphics.getWidth) 2)
       (* (/ (. board :width) 2) (. board :square-size))))
  (set offset-y
    (- (/ (love.graphics.getHeight) 2)
       (* (/ (. board :height) 2) (. board :square-size))))

  (draw-board)
  (when (. state :selected)
    (draw-highlight (. state :selected :row) (. state :selected :col))
    (each [_ move (ipairs state.moves)]
      (draw-highlight (unpack move))))
  (each [_ piece (ipairs all-pieces)]
    (draw-piece piece)))

(fn love.update [dt])

(fn love.mousepressed [x y button]
  (when (= button 1)
    (let [[row col] (pixels-to-coords x y)]
      (when (within-bounds? row col)
        (let [maybe-piece (find-piece row col)]
          (if (and state.selected
                   (find state.moves
                         (lambda [sq]
                           (= (serialize (unpack sq)) (serialize row col)))))
              (do
                (if maybe-piece
                    (capture-piece state.selected maybe-piece)
                    (move-piece state.selected row col))
                (set state.selected nil))
              (if (and maybe-piece (movable? maybe-piece))
                  (do
                    (set state.selected maybe-piece)
                    (set state.moves (movable-squares maybe-piece)))
                  (set state.selected nil)
                  (set state.moves []))))))))
