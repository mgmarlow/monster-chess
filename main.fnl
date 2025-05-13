(local colors { :light [0.94 0.85 0.71]
                :dark [0.71 0.53 0.39]
                :highlight [0.42 0.62 0.35 0.75] })

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
(var tile-size 64)

;;; General utilities

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

(fn filter [arr callback]
  (accumulate [vals [] _ item (ipairs arr)]
    (do
      (when (callback item)
        (table.insert vals item))
      vals)))

(fn coords-to-pixels [row col]
  "Convert ROW, COL to [x, y]."
  [(+ (* (- col 1) tile-size) offset-x)
   (+ (* (- row 1) tile-size) offset-y)])

(fn pixels-to-coords [x y]
  "Convert X, Y to [row, col]."
  [(math.floor (+ (/ (- y offset-y) tile-size) 1))
   (math.floor (+ (/ (- x offset-x) tile-size) 1))])

;;; State

;; Monster Chess Notation (MCN)
;;
;; From left-to-right, space-delimited:
;; - # columns
;; - # rows
;; - FEN-style board state
;; - Par score
;;
;; Example:
;; "5 5 1q3/p4/5/5/3B1 2"

(var level-1 "5 5 1q3/p4/5/5/3B1 2")
;; TODO: A little misleading, should probably be "levelstate"
(var gamestate nil)
(var selected nil)

(fn parse-mcn [mcn]
  "Expand MCN string into a table of game state."
  (let [parts (icollect [s (string.gmatch mcn "[^ ]+")] s)
        cols (tonumber (. parts 1))
        rows (tonumber (. parts 2))
        boardstr (. parts 3)
        board []
        par (tonumber (. parts 4))]
    (each [row (string.gmatch boardstr "[^/]+")]
      (let [pieces []]
        (each [char (string.gmatch row ".")]
          (if (string.match char "[0-9]")
              (for [i 1 (tonumber char)]
                (table.insert pieces "."))
              (table.insert pieces char)))
        (table.insert board pieces)))
    { :cols cols
      :rows rows
      :board board
      :par par }))

(fn attacks [piece]
  "Note that only the immediate delta is given. Consumers are expected to check
rays? to determine whether a piece can move along an entire rank/file/diagonal."
  (case piece
    {:kind :pawn :color :white} [[-1 -1] [1 -1]]
    {:kind :pawn :color :black} [[-1 1] [1 1]]
    {:kind :knight} [[-2 -1] [-2 1] [-1 -2] [-1 2] [1 -2] [1 2] [2 -1] [2 1]]
    {:kind :bishop} [[-1 -1] [-1 1] [1 -1] [1 1]]
    {:kind :rook} [[0 -1] [0 1] [-1 0] [1 0]] 
    {:kind :queen} [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]
    {:kind :king} [[-1 -1] [-1 1] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]))

(fn moves [piece]
  "Only pawn movement rules differ from capture rules."
  (case piece
    {:kind :pawn :color :white} [0 -1]
    {:kind :pawn :color :black} [0 1]
    t (attacks piece)))

(fn rays? [piece]
  "Whether the movement of PIECE continues until bounds are reached."
  (includes? [:bishop :rook :queen] (. piece :kind)))

(local state { :occupied {}
               :moves []
               :selected nil })

(fn movable? [piecestr]
  "Only white pieces are player-controlled."
  (string.match piecestr "[A-Z]"))

;; (fn remove-piece [piece]
;;   (when-let [index (find-index all-pieces piece)]
;;             (table.remove all-pieces index)))

;; (fn serialize [row col]
;;   (.. row "," col))

;; (fn regenerate-occupied-squares []
;;   (let [squares {}]
;;     (each [_ piece (ipairs all-pieces)]
;;       (tset squares (serialize (. piece :row) (. piece :col)) piece))
;;     (set state.occupied squares)))

;; (fn find-piece [row col] (. state :occupied (serialize row col)))

(fn find-piece [row col] (. gamestate :board row col))

(fn within-bounds? [row col]
  (and (>= row 0) (>= col 0) (<= col gamestate.cols) (<= row gamestate.rows)))

;; TODO: knight bug where movable wraps around at the edges
;; (fn movable-squares [piece]
;;   (let [deltas (attacks piece)
;;         [row col] [piece.row piece.col]
;;         squares []]
;;     (each [_ delta (ipairs deltas)]
;;       (let [[drow dcol] delta]
;;         (var next-row (+ row drow))
;;         (var next-col (+ col dcol))

;;         (while (within-bounds? next-row next-col)
;;           (let [other-piece (. state :occupied (serialize next-row next-col))]
;;             (when other-piece
;;               (when (= (. other-piece :color) :black)
;;                 (table.insert squares [next-row next-col]))
;;               (lua "break"))
;;             (table.insert squares [next-row next-col]))
;;           (set next-row (+ next-row drow))
;;           (set next-col (+ next-col dcol)))))
;;     squares))

;;; Actions

;; TODO: when removing movable, transition to enemy AI if there's nothing
;; left in the movable pool.

;; (fn capture-piece [winner loser]
;;   (remove-movable winner)
;;   (set winner.row (. loser :row))
;;   (set winner.col (. loser :col))
;;   (remove-piece loser)
;;   (regenerate-occupied-squares))

;; (fn move-piece [piece row col]
;;   (remove-movable piece)
;;   (set piece.row row)
;;   (set piece.col col)
;;   (regenerate-occupied-squares))

;;; Drawing

(fn draw-highlight [row col]
  (let [[x y] (coords-to-pixels row col)]
    (love.graphics.setColor (unpack colors.highlight))
    (love.graphics.rectangle "fill" x y tile-size tile-size)))

(fn draw-img [img row col]
  "Translates row/col to pixel coordinates and account for middle-of-screen offset."
  ;; 64-50 = 14, 14/2 = 7, might parameterize later
  (let [x (+ (* col tile-size) offset-x 7)
        y (+ (* row tile-size) offset-y 7)]
    (love.graphics.setColor 1 1 1)
    (love.graphics.draw img x y)))

(fn draw-square [row col]
  (let [x (* col tile-size)
        y (* row tile-size)
        light (= (% (+ row col) 2) 0)]
    (love.graphics.setColor (unpack (if light colors.light colors.dark)))
    (love.graphics.rectangle "fill" (+ x offset-x) (+ y offset-y) tile-size tile-size)))

(fn draw-board []
  (for [row 0 (- gamestate.rows 1)]
    (for [col 0 (- gamestate.cols 1)]
      (draw-square row col))))

(fn draw-pieces []
  (each [rowi row (ipairs gamestate.board)]
    (each [coli maybe-piece (ipairs row)]
      (let [rr (- rowi 1)
            cc (- coli 1)]
        (case maybe-piece
          "." nil ;; empty square
          "p" (draw-img bpawn-img rr cc)
          "P" (draw-img wpawn-img rr cc)
          "n" (draw-img bknight-img rr cc)
          "N" (draw-img wknight-img rr cc)
          "b" (draw-img bbishop-img rr cc)
          "B" (draw-img wbishop-img rr cc)
          "r" (draw-img brook-img rr cc)
          "R" (draw-img wrook-img rr cc)
          "q" (draw-img bqueen-img rr cc)
          "Q" (draw-img wqueen-img rr cc)
          "k" (draw-img bking-img rr cc)
          "K" (draw-img wking-img rr cc))))))

;;; Love handlers

(fn love.load []
  (love.window.setTitle "monster chess")
  (set gamestate (parse-mcn level-1)))

(fn love.draw []
  (set offset-x
    (- (/ (love.graphics.getWidth) 2)
       (* (/ (. gamestate :cols) 2) tile-size)))
  (set offset-y
    (- (/ (love.graphics.getHeight) 2)
       (* (/ (. gamestate :rows) 2) tile-size)))

  (draw-board)
  (when selected
    (draw-highlight selected.row selected.col)
    ;; (each [_ move (ipairs state.moves)]
    ;;   (draw-highlight (unpack move)))
    )
  (draw-pieces))

(fn love.update [dt])

(fn love.mousepressed [x y button]
  (when (= button 1)
    (let [[row col] (pixels-to-coords x y)]
      (when (within-bounds? row col)
        (let [maybe-piece (find-piece row col)]
          (if (and maybe-piece (movable? maybe-piece))
              (set selected { :row row :col col })
              (set selected nil)))))))

          ;; (if (and selected
          ;;          (find state.moves
          ;;                (lambda [sq]
          ;;                  (= (serialize (unpack sq)) (serialize row col)))))
          ;;     (do
          ;;       (if maybe-piece
          ;;           (capture-piece state.selected maybe-piece)
          ;;           (move-piece state.selected row col))
          ;;       (set state.selected nil))
          ;;     (if (and maybe-piece (movable? maybe-piece))
          ;;         (do
          ;;           (set state.selected maybe-piece)
          ;;           (set state.moves (movable-squares maybe-piece)))
          ;;         (set state.selected nil)
          ;;         (set state.moves []))))))))
