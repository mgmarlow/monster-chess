(local colors { :light [0.94 0.85 0.71]
                :dark [0.71 0.53 0.39]
                :highlight [0.42 0.62 0.35 0.75] })

(local block-font (love.graphics.newFont "fonts/Kenney Blocks.ttf" 24))
;; TODO:
;; (local editor-font (love.graphics.newFont "Arial" 24))
(local wall-img (love.graphics.newImage "img/wall.png"))
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
(var debug true)

;;; General utilities

(macro unless [condition & body]
  `(when (not ,condition)
     (do ,(unpack body))))

(macro when-let [[name condition] & body]
  `(let [,name ,condition]
     (when ,name
       (do ,(unpack body)))))

(fn includes? [arr cb]
  "Returns true if ARR has an element whose CALLBACK returns true."
  (accumulate [found nil _ item (ipairs arr)]
    (if (or found (cb item)) true found)))

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

(fn concat [t1 t2]
  (each [_ el (ipairs t2)]
    (table.insert t1 el))
  t1)

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

(var levels
     [;; Basics
      "4 4 4/1n2/4/1Rb1 2"
      "4 4 4/1n2/b3/pKb1 5"
      "4 4 2k1/1n2/4/pRb1 8"
      "4 4 4/2n1/1Qn1/2n1 6"
      ;; Introduction: walls
      "4 4 q3/xxxx/r3/1P1n 5"
      "4 4 1x1q/1nx1/3x/rKb1 6"
      "4 4 2xk/n1xx/4/R1n1 7"
      "4 4 4/q1nb/1Pxp/1p2 10"
      ;; Bigger levels
      "5 5 1x1x1/1xrx1/nx1xn/1xNx1/1xnx1 5"
      "5 5 3x1/3xr/n1rx1/1P1p1/3p1 9"
      "5 5 3nx/b2x1/2x2/1x1b1/x2qP 10"
      "5 5 rb1x1/Px1x1/qx1xr/1xnx1/1x1P1 9"
      ])

(fn parse-mcn-board [boardstr]
  "Parse MCN BOARDSTR into a 2D array of characters."
  (let [board []]
    (each [row (string.gmatch boardstr "[^/]+")]
      (let [pieces []]
        (each [char (string.gmatch row ".")]
          (if (string.match char "[0-9]")
              (for [i 1 (tonumber char)]
                (table.insert pieces "."))
              (table.insert pieces char)))
        (table.insert board pieces)))
    board))

(fn parse-mcn [mcn]
  "Expand MCN string into a table of game state."
  (let [parts (icollect [s (string.gmatch mcn "[^ ]+")] s)
        cols (tonumber (. parts 1))
        rows (tonumber (. parts 2))
        boardstr (. parts 3)
        par (tonumber (. parts 4))]
    { :cols cols
      :rows rows
      :board (parse-mcn-board boardstr)
      :par par }))

(var game { :selected nil
            :available-moves []
            :history []
            :move-counter 0
            :current-state :play
            :level-counter 1
            :level nil })

(fn fill-empty-board [rows cols]
  "Generate an empty board with n ROWS and m COLS."
  (let [board []]
    (for [_ 1 rows]
      (let [row []]
        (for [_ 1 cols]
          (table.insert row "."))
        (table.insert board row)))
    board))

;; TODO: editable rows/cols
(var editor { :rows 4
              :cols 4
              :placement nil
              :board (fill-empty-board 4 4)
              :mcn "4/4/4/4" })

(fn generate-mcn-board [board]
  "Read BOARD into an MCN string."
  (var result "")
  (each [ri row (ipairs board)]
    (var counter 0)
    (each [_ char (ipairs row)]
      (if (= char ".")
          (set counter (+ counter 1))
          (do
            (when (> counter 0)
              (set result (.. result counter))
              (set counter 0))
            (set result (.. result char)))))
    (when (> counter 0)
      (set result (.. result counter))
      (set counter 0))
    (unless (= ri (length board))
            (set result (.. result "/"))))
  result)

(fn load-level [n]
  (set game.level (parse-mcn (. levels n)))
  (set game.selected nil)
  (set game.available-moves [])
  (set game.move-counter 0)
  (set game.level-counter n)
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
    "K" [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]))

(fn moves [piece]
  "Only pawn movement rules differ from capture rules."
  (case piece
    "P" [[-1 0]]
    _ (attacks piece)))

(fn rays? [piecestr]
  "Whether the movement of PIECE continues until bounds are reached."
  (includes? ["B" "R" "Q"] (lambda [o] (= o piecestr))))

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

(fn movable-squares-pawn [row col]
  (let [squares []]
    ;; [-1 0]
    (when (and (within-bounds? (- row 1) col)
               (not (find-piece (- row 1) col)))
      (table.insert squares [(- row 1) col]))
    ;; [[-1 -1] [-1 1]]
    (when (and (within-bounds? (- row 1) (- col 1))
               (find-piece (- row 1) (- col 1)))
      (table.insert squares [(- row 1) (- col 1)]))
    (when (and (within-bounds? (- row 1) (+ col 1))
               (find-piece (- row 1) (+ col 1)))
      (table.insert squares [(- row 1) (+ col 1)]))
    squares))

(fn movable-squares-piece [piecestr row col]
  (let [deltas (moves piecestr)
        squares []]
    (each [_ delta (ipairs deltas)]
      (let [[drow dcol] delta]
        (var next-row (+ row drow))
        (var next-col (+ col dcol))
        (while (within-bounds? next-row next-col)
          (let [maybe-piece (find-piece next-row next-col)]
            (when maybe-piece
              (when (enemy? maybe-piece)
                (table.insert squares [next-row next-col]))
              (lua "break"))
            (table.insert squares [next-row next-col])
            (unless (rays? piecestr)
                (lua "break"))
            (set next-row (+ next-row drow))
            (set next-col (+ next-col dcol))))))
    squares))

(fn movable-squares [piecestr row col]
  (if (= piecestr "P")
      (movable-squares-pawn row col)
      (movable-squares-piece piecestr row col)))

(fn valid-move? [row col]
  (includes? game.available-moves
             (lambda [tuple]
               (let [[vr vc] tuple]
                 (and (= vr row) (= vc col))))))

(fn level-over? []
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
  (table.insert game.history (generate-mcn-board game.level.board))
  (set-piece row col (or (and (find-piece row col)
                              (string.upper (find-piece row col)))
                         game.selected.piece))
  (set-piece game.selected.row game.selected.col ".")
  (set game.move-counter (+ game.move-counter 1))
  (set game.selected nil))

(fn undo []
  (when-let [last-mcn (table.remove game.history (length game.history))]
            (set game.move-counter (- game.move-counter 1))
            (set game.level.board (parse-mcn-board last-mcn))))

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
    (love.graphics.setColor 1 1 1)
    (love.graphics.draw wall-img (+ x offset-x) (+ y offset-y))))

(fn draw-board [rows cols]
  (for [row 0 (- rows 1)]
    (for [col 0 (- cols 1)]
      (draw-square row col))))

;; TODO: clean this up so it can be reused betwen editor and play states
(fn draw-piece [piecestr x y]
  (love.graphics.setColor 1 1 1)
  (case piecestr
    "." nil ;; empty square
    "x" (love.graphics.draw wall-img x y)
    "p" (love.graphics.draw bpawn-img x y)
    "P" (love.graphics.draw wpawn-img x y)
    "n" (love.graphics.draw bknight-img x y)
    "N" (love.graphics.draw wknight-img x y)
    "b" (love.graphics.draw bbishop-img x y)
    "B" (love.graphics.draw wbishop-img x y)
    "r" (love.graphics.draw brook-img x y)
    "R" (love.graphics.draw wrook-img x y)
    "q" (love.graphics.draw bqueen-img x y)
    "Q" (love.graphics.draw wqueen-img x y)
    "k" (love.graphics.draw bking-img x y)
    "K" (love.graphics.draw wking-img x y)))

(fn draw-pieces [board]
  (each [rowi row (ipairs board)]
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

  (draw-board game.level.rows game.level.cols)
  (when game.selected
    (draw-highlight game.selected.row game.selected.col)
    (each [_ move (ipairs game.available-moves)]
      (draw-highlight (unpack move))))
  (love.graphics.setColor 1 1 1)
  (love.graphics.print (.. "Level: " game.level-counter) 20 10)
  (love.graphics.print (.. "Par: " game.level.par) 20 50)
  (love.graphics.print (.. "Moves: " game.move-counter) 20 90)
  (draw-pieces game.level.board))

(fn draw-level-over-state []
  (love.graphics.setColor 1 1 1)
  (love.graphics.print "Puzzle solved!" 20 10)
  (love.graphics.print (.. "Moves: " game.move-counter) 20 50)
  (love.graphics.print (.. "Par: " game.level.par) 20 90)
  (love.graphics.print "Press Enter to continue" 20 130))

(fn draw-game-over-state []
  (love.graphics.setColor 1 1 1)
  (love.graphics.print "All puzzles solved!" 20 10)
  (love.graphics.print "Thanks for playing!" 20 50))

(fn draw-editor []
  (love.graphics.setColor 1 1 1)
  (love.graphics.print "level editor" 20 10)
  ;; TODO: need a readable font for lowercase pieces.
  ;; (love.graphics.setFont editor-font)
  (love.graphics.print editor.mcn 20 (- (love.graphics.getHeight) 50))
  (love.graphics.setFont block-font)
  (draw-board editor.rows editor.cols)
  (draw-pieces editor.board)
  (when editor.placement
    (let [(x y) (love.mouse.getPosition)]
      (draw-piece editor.placement x y))))

;;; Updating

(fn update-editor [dt])

;;; Handlers

(fn handle-level-over-keypressed [key scancode isrepeat]
  (when (= key "return")
    (if (= (length levels) game.level-counter)
        (set game.current-state :game-over)
        (load-level (+ game.level-counter 1)))))

(fn handle-play-keypressed [key scancode isrepeat]
  (when (= key "r")
    (load-level game.level-counter))
  (when (= key "z")
    (undo))
  (when (and debug (= key "`"))
    (set game.current-state :editor))
  (when (and debug (= key "tab"))
    (load-level
     (if (= (length levels) game.level-counter)
         1
         (+ game.level-counter 1)))))

(local piecestrs [ "." "x" "p" "P" "n" "N" "b" "B"
                   "r" "R" "q" "Q" "k" "K" ])

;; Note: keypressed doesn't handle capital letters.
(fn handle-editor-textinput [text]
  (when (includes? piecestrs (lambda [k] (= k text)))
    (set editor.placement text)))

(fn handle-editor-keypressed [key scancode isrepeat]
  (when (= key "`")
    (load-level game.level-counter))
  (when (= key "escape")
    (set editor.placement nil)))

(fn handle-play-mousepressed [x y button]
  (when (= button 1)
    (let [[row col] (pixels-to-coords x y)]
      (when (within-bounds? row col)
        (let [maybe-piece (find-piece row col)]
          (if game.selected
              (if (valid-move? row col)
                  (do
                    (move-selected row col)
                    (when (level-over?)
                      (set game.current-state :level-over)))
                  (set game.selected nil)
                  (set game.available-moves []))
              (if (and maybe-piece (movable? maybe-piece))
                  (do
                    (set game.selected { :row row :col col :piece maybe-piece })
                    (set game.available-moves
                         (movable-squares maybe-piece row col)))
                  (set game.selected nil)
                  (set game.available-moves []))))))))

(fn handle-editor-mousepressed [x y button]
  (let [[row col] (pixels-to-coords x y)]
    (when (and editor.placement (within-bounds? row col))
      (tset editor :board row col editor.placement)
      (set editor.mcn (generate-mcn-board editor.board))
      (set editor.placement nil))))

;;; Love Entrypoints

(fn love.load []
  (love.graphics.setDefaultFilter "nearest")
  (love.window.setTitle "monster chess")
  (load-level 1))

(fn love.draw []
  (case game.current-state
    :play (draw-play-state)
    :level-over (draw-level-over-state)
    :game-over (draw-game-over-state)
    :editor (draw-editor)))

(fn love.update [dt]
  (case game.current-state
    :editor (update-editor dt)))

(fn love.textinput [text]
  (case game.current-state
    :editor (handle-editor-textinput text)))

(fn love.keypressed [key scancode isrepeat]
  (case game.current-state
    :level-over (handle-level-over-keypressed key scancode isrepeat)
    :play (handle-play-keypressed key scancode isrepeat)
    :editor (handle-editor-keypressed key scancode isrepeat)))

(fn love.mousepressed [x y button]
  (case game.current-state
    :play (handle-play-mousepressed x y button)
    :editor (handle-editor-mousepressed x y button)))
