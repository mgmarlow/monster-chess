(local colors { :light [0.94 0.85 0.71]
                :dark [0.71 0.53 0.39]
                :highlight [0.42 0.62 0.35 0.75] })

(local block-font (love.graphics.newFont "fonts/Kenney Blocks.ttf" 24))
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

(macro unless [condition & body]
  `(when (not ,condition)
     (do ,(unpack body))))

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
;;   - "x" is a impassable wall
;;   - lowercase letters are black pieces
;;   - uppercase letters are white pieces
;; - Par score
;;
;; Example:
;; "5 5 1q3/p4/5/5/3B1 2"

;; (var level-1 "5 5 1q3/p4/5/5/3B1 2")
;; TODO:
(var levels ["5 5 1q3/p4/5/5/3B1 2"
             "4 4 bxr1/2xn/1N2/4 5"])

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

(var game { :selected nil
            :available-moves []
            :move-counter 0
            :current-state :play
            :level-counter 1
            :level nil })

(fn load-level [n]
  (set game.level (parse-mcn (. levels n)))
  (set game.selected nil)
  (set game.available-moves [])
  (set game.num-moves 0)
  (set game.current-state :play))

(fn attacks [piece]
  "Note that only the immediate delta is given. Consumers are expected to check
rays? to determine whether a piece can move along an entire rank/file/diagonal."
  (case piece
    "P" [[-1 -1] [-1 1]]
    "N" [[-2 -1] [-2 1] [-1 -2] [-1 2] [1 -2] [1 2] [2 -1] [2 1]]
    "B" [[-1 -1] [-1 1] [1 -1] [1 1]]
    "R" [[0 -1] [0 1] [-1 0] [1 0]]
    "Q" [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]
    "K" [[-1 -1] [-1 1] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]))

(fn moves [piece]
  "Only pawn movement rules differ from capture rules."
  (case piece
    "P" [0 -1]
    _ (attacks piece)))

(fn rays? [piecestr]
  "Whether the movement of PIECE continues until bounds are reached."
  (includes? ["B" "R" "Q"] piecestr))

(fn movable? [piecestr]
  "Only white pieces are player-controlled."
  (string.match piecestr "[A-Z]"))

(fn enemy? [piecestr]
  (and (not= piecestr "x") (string.match piecestr "[a-z]")))

(fn find-piece [row col]
  "Check ROW, COL in game.level. If a piece is found, return the piece.
Otherwise, return false."
  (let [maybe-piece (. game :level :board row col)]
    (and (not= maybe-piece ".") maybe-piece)))

(fn within-bounds? [row col]
  (and (> row 0) (> col 0) (<= col game.level.cols) (<= row game.level.rows)))

(fn movable-squares [piecestr original-row original-col]
  (let [deltas (attacks piecestr)
        squares []]
    (each [_ delta (ipairs deltas)]
      (let [[row col] delta]
        (var next-row (+ original-row row))
        (var next-col (+ original-col col))
        ;; TODO: pawn conditions
        (while (within-bounds? next-row next-col)
          (let [maybe-piece (find-piece next-row next-col)]
            (when maybe-piece
              (when (enemy? maybe-piece)
                (table.insert squares [next-row next-col]))
              (lua "break"))
            (table.insert squares [next-row next-col])
            (unless (rays? piecestr)
                (lua "break"))
            (set next-row (+ next-row row))
            (set next-col (+ next-col col))))))
    squares))

(fn valid-move? [row col]
  (find game.available-moves
        (lambda [tuple]
          (let [[vr vc] tuple]
            (and (= vr row) (= vc col))))))

(fn game-over? []
  (let [enemies (accumulate [rst [] _ row (ipairs game.level.board)]
                  (do
                    (each [_ p (ipairs row)]
                      (when (enemy? p)
                        (table.insert rst p)))
                    rst))]
    (= (length enemies) 0)))

;;; Actions

(fn set-piece [row col value]
  (tset game :level :board row col value))

(fn move-selected [row col]
  (set-piece row col (or (and (find-piece row col)
                              (string.upper (find-piece row col)))
                         game.selected.piece))
  (set-piece game.selected.row game.selected.col ".")
  (set game.move-counter (+ game.move-counter 1))
  (set game.selected nil))

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

(fn draw-wall [row col]
  (let [x (* col tile-size)
        y (* row tile-size)]
    (love.graphics.setColor 0 0 0)
    (love.graphics.rectangle "fill" (+ x offset-x) (+ y offset-y) tile-size tile-size)))

(fn draw-board []
  (for [row 0 (- game.level.rows 1)]
    (for [col 0 (- game.level.cols 1)]
      (draw-square row col))))

(fn draw-pieces []
  (each [rowi row (ipairs game.level.board)]
    (each [coli maybe-piece (ipairs row)]
      (let [rr (- rowi 1)
            cc (- coli 1)]
        (case maybe-piece
          "." nil ;; empty square
          "x" (draw-wall rr cc)
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

(fn draw-play-state []
  (love.graphics.setFont block-font)
  (set offset-x
    (- (/ (love.graphics.getWidth) 2)
       (* (/ game.level.cols 2) tile-size)))
  (set offset-y
    (- (/ (love.graphics.getHeight) 2)
       (* (/ game.level.rows 2) tile-size)))

  (draw-board)
  (when game.selected
    (draw-highlight game.selected.row game.selected.col)
    (each [_ move (ipairs game.available-moves)]
      (draw-highlight (unpack move))))
  (love.graphics.setColor 1 1 1)
  (love.graphics.print (.. "Moves: " game.move-counter) 10 10)
  (draw-pieces))

(fn draw-game-over-state []
  (love.graphics.setColor 1 1 1)
  (love.graphics.print "Puzzle solved!" 10 10)
  (love.graphics.print (.. "Moves: " game.move-counter) 10 50)
  (love.graphics.print (.. "Par: " game.level.par) 10 90)
  (love.graphics.print "Press Enter to continue" 10 130))

;;; Love handlers

(fn love.load []
  (print game.current-state)
  (love.window.setTitle "monster chess")
  (load-level 2))

(fn love.draw []
  (case game.current-state
    :play (draw-play-state)
    :game-over (draw-game-over-state)))

(fn love.update [dt])

(fn love.keypressed [key scancode isrepeat]
  (case game.current-state
    :game-over (do
                 (when (= key "return")
                   ;; TODO:
                   (print "go to next level")))
    :play (do
            (when (= key "r")
              (print "reset level")))))

(fn love.mousepressed [x y button]
  ;; Kind of a shortcut/hack to avoid checking states here.
  (unless (= game.current-state :play)
          (lua "return"))

  (when (= button 1)
    (let [[row col] (pixels-to-coords x y)]
      (when (within-bounds? row col)
        (let [maybe-piece (find-piece row col)]
          (if game.selected
              (if (valid-move? row col)
                  (do
                    (move-selected row col)
                    (when (game-over?)
                      (set game.current-state :game-over)))
                  (set game.selected nil)
                  (set game.available-moves []))
              (if (and maybe-piece (movable? maybe-piece))
                  (do
                    (set game.selected { :row row :col col :piece maybe-piece })
                    (set game.available-moves
                         (movable-squares maybe-piece row col)))
                  (set game.selected nil)
                  (set game.available-moves []))))))))
